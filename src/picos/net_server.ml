module Sem = Picos_std_sync.Semaphore.Counting

type client_handler = Unix.sockaddr -> IO_in.t -> IO_out.t -> unit

type t = {
  active: bool Atomic.t;
  sock: Unix.file_descr;
  client_handler: client_handler;
  spawn: (unit -> unit) -> unit Picos.Computation.t;
  max_conns: int;
  sem: Sem.t;
  mutable running: unit Picos.Computation.t option;
  exn_handler: exn -> Printexc.raw_backtrace -> unit;
}

let[@inline] join (self : t) : unit =
  Option.iter Picos.Computation.await self.running

let[@inline] max_connections self = self.max_conns

let[@inline] n_active_connections (self : t) : int =
  self.max_conns - Sem.get_value self.sem

let[@inline] running (self : t) : bool = Atomic.get self.active
let shutdown (self : t) = if Atomic.exchange self.active false then ()

open struct
  let default_exn_handler exn bt =
    Printf.eprintf "uncaught exception in network server: %s\n%s%!"
      (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string bt)

  let run (self : t) () : unit =
    while Atomic.get self.active do
      let client_sock, client_addr = Base.accept self.sock in
      Sem.acquire self.sem;

      let cleanup () =
        (try Unix.shutdown client_sock Unix.SHUTDOWN_ALL with _ -> ());
        (* TODO: close in nanoev too *)
        (try Unix.close client_sock with _ -> ());
        Sem.release self.sem
      in

      let comp : _ Picos.Computation.t =
        self.spawn (fun () ->
            let ic = IO_in.of_unix_fd client_sock in
            let oc = IO_out.of_unix_fd client_sock in
            try
              self.client_handler client_addr ic oc;
              cleanup ()
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              cleanup ();
              self.exn_handler exn bt)
      in
      ignore (comp : _ Picos.Computation.t)
    done
end

let establish ?backlog ?max_connections ?(exn_handler = default_exn_handler)
    ~spawn ~(client_handler : client_handler) addr : t =
  let ev =
    match Atomic.get Global_.st with
    | Some { nanoev = ev; _ } -> ev
    | None -> invalid_arg "Nanoev_picos.Net_server: no event loop installed"
  in

  let max_connections =
    match max_connections with
    | None -> Nanoev.max_fds ev
    | Some n -> min (Nanoev.max_fds ev) n
  in
  let sem = Sem.make max_connections in

  let backlog =
    match backlog with
    | Some n -> max 4 n
    | None -> max 4 max_connections
  in

  let domain = Unix.domain_of_sockaddr addr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in

  Unix.bind sock addr;
  Unix.listen sock backlog;
  Unix.set_nonblock sock;
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  (try Unix.setsockopt sock Unix.TCP_NODELAY true with _ -> ());

  let server =
    {
      active = Atomic.make true;
      max_conns = max_connections;
      sem;
      spawn;
      sock;
      client_handler;
      running = None;
      exn_handler;
    }
  in

  server.running <- Some (spawn (run server));
  server

let with_ ?backlog ?max_connections ?exn_handler ~spawn ~client_handler addr f =
  let server =
    establish ?backlog ?max_connections ?exn_handler ~spawn ~client_handler addr
  in
  Fun.protect ~finally:(fun () -> shutdown server) (fun () -> f server)
