open Common_

let connect addr : Unix.file_descr =
  let sock = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.set_nonblock sock;
  (try Unix.setsockopt sock Unix.TCP_NODELAY true with _ -> ());

  (* connect asynchronously *)
  Base.connect sock addr;
  sock

let with_connect addr (f : IO_in.t -> IO_out.t -> 'a) : 'a =
  let sock = connect addr in

  let ic = IO_in.of_unix_fd sock in
  let oc = IO_out.of_unix_fd sock in

  let finally () =
    (try Unix.shutdown sock Unix.SHUTDOWN_ALL with _ -> ());
    try Unix.close sock with _ -> ()
  in
  let@ () = Fun.protect ~finally in
  f ic oc
