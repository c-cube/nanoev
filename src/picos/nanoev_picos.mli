(** Basic interface with picos *)

val setup_bg_thread : Nanoev.t -> unit
(** Install this event loop in a background thread *)

val has_bg_thread : unit -> bool
(** [has_bg_thread ()] is [true] iff a background thread is running a nanoev loop *)

(** {2 Non blocking IO primitives} *)

val read : Unix.file_descr -> bytes -> int -> int -> int
val connect : Unix.file_descr -> Unix.sockaddr -> unit
val write : Unix.file_descr -> bytes -> int -> int -> int
val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
val sleep : float -> unit
