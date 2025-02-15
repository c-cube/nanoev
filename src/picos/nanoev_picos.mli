(** Basic interface with picos *)

val setup_bg_thread : Nanoev.t -> unit
(** Install this event loop in a background thread *)

val has_bg_thread : unit -> bool
(** [has_bg_thread ()] is [true] iff a background thread is running a nanoev loop *)

(** {2 Non blocking IO primitives} *)

val read : Unix.file_descr -> bytes -> int -> int -> int
(** Read from the non blocking FD.
  @raise Nanoev.Closed if the FD is closed
  @raise Unix.Unix_error for other errors *)

val write : Unix.file_descr -> bytes -> int -> int -> int
(** Write into the non blocking FD.
  @raise Nanoev.Closed if the FD is closed
  @raise Unix.Unix_error for other errors *)

val close : Unix.file_descr -> unit
(** Close the file descriptor
  @raise Unix.Unix_error when it fails *)

val connect : Unix.file_descr -> Unix.sockaddr -> unit

val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** Accept a connection on this fd.
  @raise Nanoev.Closed if the FD is closed.
  @raise Unix.Unix_error for other errors *)

val sleep : float -> unit
