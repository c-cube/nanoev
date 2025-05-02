module F = Moonpool_fib
module IO = Nanoev_picos
module Trace = Trace_core

[@@@ocaml.alert "-deprecated"]

let ( let@ ) = ( @@ )
let pf = Printf.printf
let spf = Printf.sprintf
let verbose = ref false

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

let main ~port ~unix_sock ~runner () =
  pf "serve on %s\n%!"
    (if unix_sock = "" then
       spf "localhost:%d" port
     else
       spf "unix:%S" unix_sock);

  let addr =
    if unix_sock = "" then
      Unix.ADDR_INET (Unix.inet_addr_loopback, port)
    else
      Unix.ADDR_UNIX unix_sock
  in

  let server =
    IO.Net_server.establish addr
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
          let n = Iostream.In.input ic buf 0 (Bytes.length buf) in
          if n = 0 then
            continue := false
          else (
            Iostream.Out.output oc buf 0 n;
            Iostream.Out_buf.flush oc
          )
        done;

        Trace.exit_manual_span _sp;
        if !verbose then
          pf "done with client on %s\n%!" (str_of_sockaddr client_addr))
  in
  IO.Net_server.join server;
  IO.Net_server.shutdown server;
  print_endline "exit"

let () =
  let@ () = Trace_tef.with_setup () in
  let port = ref 1234 in
  let unix_sock = ref "" in
  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "--unix", Arg.Set_string unix_sock, " unix socket";
      "-v", Arg.Set verbose, " verbose";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo_server";

  let@ () =
    Nanoev_picos.Background_thread.with_setup (Nanoev_posix.create ())
  in
  F.main @@ fun runner -> main ~port:!port ~unix_sock:!unix_sock ~runner ()
