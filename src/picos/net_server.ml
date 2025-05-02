type client_handler = Unix.sockaddr -> IO_in.t -> IO_out.t -> unit

type t = {
  active: bool Atomic.t;
  sock: Unix.file_descr;
  client_handler: client_handler;
  spawn: (unit -> unit) -> unit Picos.Computation.t;
  mutable running: unit Picos.Computation.t option;
}

let join (self : t) : unit = Option.iter Picos.Computation.await self.running
let shutdown (self : t) = if Atomic.exchange self.active false then ()

open struct
  let run (self : t) () : unit =
    while Atomic.get self.active do
      let client_sock, client_addr = Base.accept self.sock in
      let comp =
        self.spawn (fun () ->
            let ic = IO_in.of_unix_fd client_sock in
            let oc = IO_out.of_unix_fd client_sock in
            self.client_handler client_addr ic oc)
      in
      ignore (comp : _ Picos.Computation.t)
    done
end

let establish ?(backlog = 32) ~spawn ~(client_handler : client_handler) addr : t
    =
  let domain = Unix.domain_of_sockaddr addr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  Unix.bind sock addr;
  Unix.listen sock backlog;
  Unix.set_nonblock sock;
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  (try Unix.setsockopt sock Unix.TCP_NODELAY true with _ -> ());

  let server =
    { active = Atomic.make true; spawn; sock; client_handler; running = None }
  in

  server.running <- Some (spawn (run server));
  server

let with_ ?backlog ~spawn ~client_handler addr f =
  let server = establish ?backlog ~spawn ~client_handler addr in
  Fun.protect ~finally:(fun () -> shutdown server) (fun () -> f server)
