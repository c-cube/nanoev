type client_handler = Unix.sockaddr -> IO_in.t -> IO_out.t -> unit
type t

val join : t -> unit
val shutdown : t -> unit

val establish :
  ?backlog:int ->
  spawn:((unit -> unit) -> unit Picos.Computation.t) ->
  client_handler:client_handler ->
  Unix.sockaddr ->
  t

val with_ :
  ?backlog:int ->
  spawn:((unit -> unit) -> unit Picos.Computation.t) ->
  client_handler:client_handler ->
  Unix.sockaddr ->
  (t -> 'a) ->
  'a
