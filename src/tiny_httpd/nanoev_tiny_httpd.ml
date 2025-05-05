module TH = Tiny_httpd_core
module EV = Nanoev_picos
module Log = TH.Log
module Slice = Iostream.Slice
module Pool = TH.Pool
module Buf = TH.Buf

module Sem_ = Picos_std_sync.Semaphore.Counting
(** Non blocking semaphore *)

module Out = struct
  open Iostream

  class type t = Out_buf.t

  class of_unix_fd ?(close_noerr = false) ~closed ~(buf : Slice.t)
    (fd : Unix.file_descr) :
    t =
    object
      inherit Out_buf.t_from_output ~bytes:buf.bytes ()

      method private output_underlying bs i len0 =
        let i = ref i in
        let len = ref len0 in
        while !len > 0 do
          match EV.write_once fd bs !i !len with
          | 0 -> failwith "write failed"
          | n ->
            i := !i + n;
            len := !len - n
          | exception
              Unix.Unix_error
                ( (( Unix.EBADF | Unix.ENOTCONN | Unix.ESHUTDOWN
                   | Unix.ECONNRESET | Unix.EPIPE ) as err),
                  fn,
                  _ ) ->
            failwith
            @@ Printf.sprintf "write failed in %s: %s" fn
                 (Unix.error_message err)
        done

      method private close_underlying () =
        if not (Atomic.exchange closed true) then
          if close_noerr then (
            try EV.close fd with _ -> ()
          ) else
            EV.close fd
    end
end

module In = struct
  open Iostream

  class type t = In_buf.t

  let of_unix_fd ?(close_noerr = false) ~closed ~(buf : Slice.t)
      (fd : Unix.file_descr) : t =
    let eof = ref false in
    object
      inherit Iostream.In_buf.t_from_refill ~bytes:buf.bytes ()

      method private refill (slice : Slice.t) =
        if not !eof then (
          slice.off <- 0;
          let continue = ref true in
          while !continue do
            match EV.read fd slice.bytes 0 (Bytes.length slice.bytes) with
            | n ->
              slice.len <- n;
              continue := false
            | exception
                Unix.Unix_error
                  ( ( Unix.EBADF | Unix.ENOTCONN | Unix.ESHUTDOWN
                    | Unix.ECONNRESET | Unix.EPIPE ),
                    _,
                    _ ) ->
              eof := true;
              continue := false
          done;
          (* Printf.eprintf "read returned %d B\n%!" !n; *)
          if slice.len = 0 then eof := true
        )

      method close () =
        if not (Atomic.exchange closed true) then (
          eof := true;
          if close_noerr then (
            try EV.close fd with _ -> ()
          ) else
            EV.close fd
        )
    end
end

module Unix_tcp_server_ = struct
  let get_addr_ sock =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (addr, port) -> addr, port
    | _ -> invalid_arg "httpd: address is not INET"

  type t = {
    addr: string;
    port: int;
    buf_pool: Buf.t Pool.t;
    slice_pool: Slice.t Pool.t;
    max_connections: int;
    sem_max_connections: Sem_.t;
        (** semaphore to restrict the number of active concurrent connections *)
    mutable sock: Unix.file_descr option;  (** Socket *)
    new_thread: (unit -> unit) -> unit;
    timeout: float;
    masksigpipe: bool;
    running: bool Atomic.t;
  }

  let shutdown_silent_ fd =
    try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ()

  let close_silent_ fd = try Unix.close fd with _ -> ()

  let to_tcp_server (self : t) : TH.IO.TCP_server.builder =
    {
      TH.IO.TCP_server.serve =
        (fun ~after_init ~handle () : unit ->
          if self.masksigpipe && not Sys.win32 then
            ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ] : _ list);
          let sock, should_bind =
            match self.sock with
            | Some s ->
              s, false
              (* Because we're getting a socket from the caller (e.g. systemd) *)
            | None ->
              let s =
                Unix.socket
                  (if TH.Util.is_ipv6_str self.addr then
                     Unix.PF_INET6
                   else
                     Unix.PF_INET)
                  Unix.SOCK_STREAM 0
              in
              s, true (* Because we're creating the socket ourselves *)
          in
          Unix.set_nonblock sock;
          Unix.setsockopt_optint sock Unix.SO_LINGER None;
          if should_bind then (
            let inet_addr = Unix.inet_addr_of_string self.addr in
            Unix.setsockopt sock Unix.SO_REUSEADDR true;
            Unix.bind sock (Unix.ADDR_INET (inet_addr, self.port));
            let n_listen = self.max_connections in
            Unix.listen sock n_listen
          );

          self.sock <- Some sock;

          let tcp_server =
            {
              TH.IO.TCP_server.stop =
                (fun () ->
                  Atomic.set self.running false;

                  (* close accept socket so the main loop will return *)
                  try Unix.close sock with _ -> ());
              running = (fun () -> Atomic.get self.running);
              active_connections =
                (fun () ->
                  self.max_connections - Sem_.get_value self.sem_max_connections);
              endpoint =
                (fun () ->
                  let addr, port = get_addr_ sock in
                  Unix.string_of_inet_addr addr, port);
            }
          in
          after_init tcp_server;

          (* how to handle a single client *)
          let handle_client_ (client_sock : Unix.file_descr)
              (client_addr : Unix.sockaddr) : unit =
            Log.debug (fun k ->
                k "t[%d]: serving new client on %s"
                  (Thread.id @@ Thread.self ())
                  (TH.Util.show_sockaddr client_addr));

            if self.masksigpipe && not Sys.win32 then
              ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ] : _ list);
            Unix.set_nonblock client_sock;
            Unix.setsockopt client_sock Unix.TCP_NODELAY true;
            Unix.(setsockopt_float client_sock SO_RCVTIMEO self.timeout);
            Unix.(setsockopt_float client_sock SO_SNDTIMEO self.timeout);

            Pool.with_resource self.slice_pool @@ fun ic_buf ->
            Pool.with_resource self.slice_pool @@ fun oc_buf ->
            let closed = Atomic.make false in

            let oc =
              new Out.of_unix_fd
                ~close_noerr:true ~closed ~buf:oc_buf client_sock
            in
            let ic =
              In.of_unix_fd ~close_noerr:true ~closed ~buf:ic_buf client_sock
            in
            handle.handle ~client_addr ic oc
          in

          Unix.set_nonblock sock;
          while Atomic.get self.running do
            match EV.accept sock with
            | client_sock, client_addr ->
              (* limit concurrency *)
              Sem_.acquire self.sem_max_connections;
              (* Block INT/HUP while cloning to avoid children handling them.
                 When thread gets them, our Unix.accept raises neatly. *)
              if not Sys.win32 then
                ignore Unix.(sigprocmask SIG_BLOCK Sys.[ sigint; sighup ]);
              self.new_thread (fun () ->
                  try
                    handle_client_ client_sock client_addr;
                    Log.debug (fun k ->
                        k "t[%d]: done with client on %s, exiting"
                          (Thread.id @@ Thread.self ())
                        @@ TH.Util.show_sockaddr client_addr);
                    shutdown_silent_ client_sock;
                    close_silent_ client_sock;
                    Sem_.release self.sem_max_connections
                  with e ->
                    let bt = Printexc.get_raw_backtrace () in
                    shutdown_silent_ client_sock;
                    close_silent_ client_sock;
                    Sem_.release self.sem_max_connections;
                    Log.error (fun k ->
                        k
                          "@[<v>Handler: uncaught exception for client %s:@ \
                           %s@ %s@]"
                          (TH.Util.show_sockaddr client_addr)
                          (Printexc.to_string e)
                          (Printexc.raw_backtrace_to_string bt)));
              if not Sys.win32 then
                ignore Unix.(sigprocmask SIG_UNBLOCK Sys.[ sigint; sighup ])
            | exception e ->
              Log.error (fun k ->
                  k "Unix.accept raised an exception: %s" (Printexc.to_string e))
          done;

          (* Wait for all threads to be done: this only works if all threads are done. *)
          (try Unix.close sock with _ -> ());
          (* TODO? *)
          (* Sem_.acquire self.sem_max_connections.max self.sem_max_connections; *)
          ());
    }
end

open struct
  let get_max_connection_ ?(max_connections = 2048) () : int =
    let max_connections = min (max 4 @@ EV.max_fds ()) max_connections in
    max_connections

  let clear_slice (slice : Slice.t) =
    Bytes.fill slice.bytes 0 (Bytes.length slice.bytes) '\x00';
    slice.off <- 0;
    slice.len <- 0
end

let create ?enable_logging ?(masksigpipe = not Sys.win32) ?max_connections
    ?max_buf_pool_size ?(timeout = 0.0) ?buf_size
    ?(get_time_s = Unix.gettimeofday) ?(addr = "127.0.0.1") ?(port = 8080) ?sock
    ?head_middlewares ?middlewares ~new_thread () : TH.Server.t =
  let max_connections = get_max_connection_ ?max_connections () in
  let max_pool_size =
    match max_buf_pool_size with
    | None -> min 4096 max_connections * 2
    | Some m -> m
  in
  let server =
    {
      Unix_tcp_server_.addr;
      new_thread;
      buf_pool =
        Pool.create ~clear:Buf.clear_and_zero ~max_size:max_pool_size
          ~mk_item:(fun () -> Buf.create ?size:buf_size ())
          ();
      slice_pool =
        Pool.create ~clear:clear_slice
          ~mk_item:
            (let buf_size = Option.value buf_size ~default:4096 in
             fun () -> Slice.create buf_size)
          ();
      running = Atomic.make true;
      port;
      sock;
      max_connections;
      sem_max_connections = Sem_.make max_connections;
      masksigpipe;
      timeout;
    }
  in
  let tcp_server_builder = Unix_tcp_server_.to_tcp_server server in
  let module B = struct
    let init_addr () = addr
    let init_port () = port
    let get_time_s = get_time_s
    let tcp_server () = tcp_server_builder
  end in
  let backend = (module B : TH.Server.IO_BACKEND) in
  TH.Server.create_from ?enable_logging ?buf_size ?head_middlewares ?middlewares
    ~backend ()
