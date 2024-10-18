(** Nano event loop *)

(*
module type BACKEND = Intf.BACKEND

val unix : unit -> (module BACKEND)
val create : ?backend:(module BACKEND) -> unit -> t
*)

type t

val create : unit -> t

val clear : t -> unit
(** Reset the state *)

val wakeup_from_outside : t -> unit

val step : t -> unit
(** Run one step of the event loop until something happens *)

val on_readable : t -> Unix.file_descr -> 'a -> ('a -> unit) -> unit
val on_writable : t -> Unix.file_descr -> 'a -> ('a -> unit) -> unit
val run_after_s : t -> float -> 'a -> ('a -> unit) -> unit
