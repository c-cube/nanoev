(** Basic interface with picos *)

module FD = Nanoev.FD

val setup_bg_thread : Nanoev.t -> unit
(** Install this event loop in a background thread *)

val has_bg_thread : unit -> bool
(** [has_bg_thread ()] is [true] iff a background thread is running a nanoev loop *)

(** {2 Non blocking IO primitives} *)

val get_fd : Unix.file_descr -> FD.t
val read : FD.t -> bytes -> int -> int -> int
val connect : FD.t -> Unix.sockaddr -> unit
val write : FD.t -> bytes -> int -> int -> int
val accept : FD.t -> FD.t * Unix.sockaddr
val sleep : float -> unit
