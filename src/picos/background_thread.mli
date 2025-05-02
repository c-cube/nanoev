val setup : Nanoev.t -> unit
(** Install this event loop in a background thread *)

val shutdown : unit -> unit
(** Shutdown background thread, assuming {! is_setup} returns [true] *)

val with_setup : Nanoev.t -> (unit -> 'a) -> 'a

val is_setup : unit -> bool
(** [is_setup()] is [true] iff a background thread is running a nanoev loop *)
