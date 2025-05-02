type client_handler = Unix.sockaddr -> IO_in.t -> IO_out.t -> unit
type t

val join : t -> unit
(** Wait for server to shutdown *)

val shutdown : t -> unit
(** Ask the server to stop *)

val running : t -> bool
val max_connections : t -> int
val n_active_connections : t -> int

val establish :
  ?backlog:int ->
  ?max_connections:int ->
  ?exn_handler:(exn -> Printexc.raw_backtrace -> unit) ->
  spawn:((unit -> unit) -> unit Picos.Computation.t) ->
  client_handler:client_handler ->
  Unix.sockaddr ->
  t
(** Create and start a new server on the given socket address.
    @param spawn used to spawn a new computation per client
    @param client_handler
      the logic for talking to a client, will run in its own computation
    @param backlog number of connections waiting in the listening socket
    @param max_connections max number of simultaneous connections *)

val with_ :
  ?backlog:int ->
  ?max_connections:int ->
  ?exn_handler:(exn -> Printexc.raw_backtrace -> unit) ->
  spawn:((unit -> unit) -> unit Picos.Computation.t) ->
  client_handler:client_handler ->
  Unix.sockaddr ->
  (t -> 'a) ->
  'a
