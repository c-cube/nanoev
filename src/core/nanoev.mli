(** Nano event loop.

    {1 Summary}

    Nanoev is a small abstraction over non-blocking IO event loops offered by
    most operating systems (e.g kqueue on BSD/macOS, poll on posix systems,
    epoll on linux, completion ports on windows).

    The idea is that a [Nanoev.t] encapsulates a single event loop in a reusable
    form, and can be used in higher-level concurrency packages or to write async
    IO code directly. See {!Nanoev_unix} for a [Unix.select]-based
    implementation and {!Nanoev_posix} for a [poll]-based (better)
    implementation. *)

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
  (** A nanoev event loop provides a set of operations on a hidden state ['st].
  *)

  val build : 'a ops -> 'a -> t
  (** Build a [Nanoev.t] from operations and hidden state. *)
end

val clear : t -> unit
(** Reset the state, similar to creating a new event loop from scratch *)

val wakeup_from_outside : t -> unit
(** Wakeup a sleeping event loop from the outside (another thread, a signal
    handler, etc.). This must be thread-safe. *)

val step : t -> unit
(** Run one step of the event loop until something happens. This can potentially
    block the caller for an interdeterminate duration. *)

val close : t -> Unix.file_descr -> unit
(** Close the file descriptor and clean it up inside the event loop. Callbacks
    registered on this FD will be called with [~closed:true]. *)

val max_fds : t -> int
(** Maximum number of file descriptors that can be observed at once. This
    depends on the syscall underlying the event loop, as well as other system
    limits (see [ulimit -n] to change this limit on linux, for example). *)

val on_readable :
  t -> Unix.file_descr -> 'a -> 'b -> (closed:bool -> 'a -> 'b -> unit) -> unit
(** [on_readable nanoev fd x y f] registers [f x y] to be called when [fd]
    becomes readable. An additional [closed] parameter is passed to [f] to
    inform it of whether [fd] is {i known} to be closed. *)

val on_writable :
  t -> Unix.file_descr -> 'a -> 'b -> (closed:bool -> 'a -> 'b -> unit) -> unit
(** Same as {!on_readable} but calls [f] when the FD is writable. *)

val run_after_s : t -> float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit
(** [run_after_s nanoev [duration] x y f] registers [f x y] to be called after
    [duration] seconds have elapsed. *)

(**/**)

module Trace_ = Trace_

(**/**)
