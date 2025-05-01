(** Basic interface with picos *)

module Background_thread : sig
  val setup : Nanoev.t -> unit
  (** Install this event loop in a background thread *)

  val shutdown : unit -> unit
  (** Shutdown background thread, assuming {! is_setup} returns [true] *)

  val with_setup : Nanoev.t -> (unit -> 'a) -> 'a

  val is_setup : unit -> bool
  (** [is_setup()] is [true] iff a background thread is running a nanoev loop *)
end

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
(** Connect this FD to the remote address.
    @raise Nanoev.Closed if the FD is closed.
    @raise Unix.Unix_error for other errors *)

val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** Accept a connection on this fd.
    @raise Nanoev.Closed if the FD is closed.
    @raise Unix.Unix_error for other errors *)

val max_fds : unit -> int
(** Maximum number of file descriptors one can await on. See {!Nanoev.max_fds}
*)

val sleep : float -> unit
(** Suspend current fiber for [n] seconds *)
