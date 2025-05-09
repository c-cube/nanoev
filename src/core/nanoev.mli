(** Nano event loop *)

type t

exception Closed

module Impl : sig
  type 'st ops = {
    clear: 'st -> unit;
    wakeup_from_outside: 'st -> unit;
    close: 'st -> Unix.file_descr -> unit;
    max_fds: 'st -> int;
    on_readable:
      'a 'b.
      'st ->
      Unix.file_descr ->
      'a ->
      'b ->
      (closed:bool -> 'a -> 'b -> unit) ->
      unit;
    on_writable:
      'a 'b.
      'st ->
      Unix.file_descr ->
      'a ->
      'b ->
      (closed:bool -> 'a -> 'b -> unit) ->
      unit;
    run_after_s: 'a 'b. 'st -> float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    step: 'st -> unit;
  }

  val build : 'a ops -> 'a -> t
end

val clear : t -> unit
(** Reset the state *)

val wakeup_from_outside : t -> unit

val step : t -> unit
(** Run one step of the event loop until something happens *)

val close : t -> Unix.file_descr -> unit
(** Close the file descriptor and clean it up *)

val max_fds : t -> int
(** Maximum number of file descriptors that can be observed at once. *)

val on_readable :
  t -> Unix.file_descr -> 'a -> 'b -> (closed:bool -> 'a -> 'b -> unit) -> unit

val on_writable :
  t -> Unix.file_descr -> 'a -> 'b -> (closed:bool -> 'a -> 'b -> unit) -> unit

val run_after_s : t -> float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit

(**/**)

module Trace_ = Trace_

(**/**)
