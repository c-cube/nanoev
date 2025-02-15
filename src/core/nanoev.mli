(** Nano event loop *)

type t

module FD : sig
  type t = private {
    fd: Unix.file_descr;
    closed: bool Atomic.t;
  }

  val close : t -> unit
end

module Impl : sig
  type 'st ops = {
    clear: 'st -> unit;
    get_fd: 'st -> Unix.file_descr -> FD.t;
    wakeup_from_outside: 'st -> unit;
    on_readable: 'a 'b. 'st -> FD.t -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    on_writable: 'a 'b. 'st -> FD.t -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    run_after_s: 'a 'b. 'st -> float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    step: 'st -> unit;
  }

  val build : 'a ops -> 'a -> t
end

val clear : t -> unit
(** Reset the state *)

val wakeup_from_outside : t -> unit
val get_fd : t -> Unix.file_descr -> FD.t

val step : t -> unit
(** Run one step of the event loop until something happens *)

val on_readable : t -> FD.t -> 'a -> 'b -> ('a -> 'b -> unit) -> unit
val on_writable : t -> FD.t -> 'a -> 'b -> ('a -> 'b -> unit) -> unit
val run_after_s : t -> float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit

(**/**)

module Trace_ = Trace_

(**/**)
