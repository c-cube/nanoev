val read : Unix.file_descr -> bytes -> int -> int -> int
(** Read from the non blocking FD.
    @raise Nanoev.Closed if the FD is closed
    @raise Unix.Unix_error for other errors *)

val write_once : Unix.file_descr -> bytes -> int -> int -> int
(** Write into the non blocking FD.
    @raise Nanoev.Closed if the FD is closed
    @raise Unix.Unix_error for other errors *)

val write : Unix.file_descr -> bytes -> int -> int -> unit

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

module Raw : sig
  val run_after_s : float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit

  val on_writable :
    Unix.file_descr -> 'a -> 'b -> (closed:bool -> 'a -> 'b -> unit) -> unit

  val on_readable :
    Unix.file_descr -> 'a -> 'b -> (closed:bool -> 'a -> 'b -> unit) -> unit

  val retry_read : Unix.file_descr -> (unit -> 'a) -> 'a
  val retry_write : Unix.file_descr -> (unit -> 'a) -> 'a
end
