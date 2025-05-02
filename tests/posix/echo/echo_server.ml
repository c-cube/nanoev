module F = Moonpool_fib
module IO = Nanoev_picos
module Trace = Trace_core

[@@@ocaml.alert "-deprecated"]

let ( let@ ) = ( @@ )
let pf = Printf.printf
let spf = Printf.sprintf
let verbose = ref false
let n_reply_response = Atomic.make 0

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

let main ~port ~unix_sock ~max_conns ~runner () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  pf "serve on %s\n%!"
    (if unix_sock = "" then
       spf "localhost:%d" port
     else
       spf "unix:%S" unix_sock);

  let addr =
    if unix_sock = "" then
      Unix.ADDR_INET (Unix.inet_addr_loopback, port)
    else (
      (* remove leftover unix socket file, if any *)
      (try Sys.remove unix_sock with _ -> ());
      Unix.ADDR_UNIX unix_sock
    )
  in

  let server =
    IO.Net_server.establish ?max_connections:max_conns addr
      ~spawn:(fun f -> Moonpool.spawn ~on:runner f)
      ~client_handler:(fun client_addr ic oc ->
        let _sp =
          Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "serve"
        in

        if !verbose then
          pf "handle client on %s\n%!" (str_of_sockaddr client_addr);

        let buf = Bytes.create 256 in
        let continue = ref true in
        while !continue do
          match Iostream.In.input ic buf 0 (Bytes.length buf) with
          | exception exn ->
            continue := false;
            Printf.eprintf "error in client handler: %s\n%!"
              (Printexc.to_string exn)
          | 0 -> continue := false
          | n ->
            Atomic.incr n_reply_response;
            Iostream.Out.output oc buf 0 n;
            Iostream.Out_buf.flush oc;
            Picos.Fiber.yield ()
        done;

        Trace.exit_manual_span _sp;
        if !verbose then
          pf "done with client on %s\n%!" (str_of_sockaddr client_addr))
  in

  Printf.printf "max number of connections: %d\n%!"
    (IO.Net_server.max_connections server);

  if Trace.enabled () then
    ignore
      (Thread.create
         (fun () ->
           while IO.Net_server.running server do
             Trace.counter_int ~level:Info "n-conns"
               (IO.Net_server.n_active_connections server);
             let gc = Gc.quick_stat () in
             Trace.counter_int ~level:Info "gc.major" gc.major_collections;
             Trace.counter_int ~level:Info "gc.minor" gc.minor_collections;
             Trace.counter_int ~level:Info "n-reply-response"
               (Atomic.get n_reply_response);
             Trace.counter_int ~level:Info "gc.heap-size" (gc.heap_words * 64);

             Thread.delay 0.2
           done)
         ()
        : Thread.t);

  IO.Net_server.join server;
  IO.Net_server.shutdown server;
  print_endline "exit"

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_current_level Info;
  let port = ref 1234 in
  let unix_sock = ref "" in
  let max_conns = ref None in
  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "--unix", Arg.Set_string unix_sock, " unix socket";
      ( "--max-conns",
        Arg.Int (fun i -> max_conns := Some i),
        " max number of connections" );
      "-v", Arg.Set verbose, " verbose";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo_server";

  let@ () =
    Nanoev_picos.Background_thread.with_setup (Nanoev_posix.create ())
  in
  F.main @@ fun runner ->
  main ~port:!port ~unix_sock:!unix_sock ~max_conns:!max_conns ~runner ()
