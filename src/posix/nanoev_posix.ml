open struct
  module Trace_ = Nanoev.Trace_

  let ( let@ ) = ( @@ )
  let now_ns : unit -> int64 = Mtime_clock.now_ns
  let[@inline] ns_of_s (t : float) : int64 = Int64.of_float (t *. 1e9)
  let[@inline] ns_to_s (t : int64) : float = Int64.to_float t /. 1e9
end

module Fd_tbl = Hashtbl.Make (struct
  open Iomux.Util

  type t = Unix.file_descr

  let equal a b = Int.equal (fd_of_unix a) (fd_of_unix b)
  let hash a = Hashtbl.hash (fd_of_unix a)
end)

module P = Iomux.Poll
module Flags = P.Flags

module Sync_queue = struct
  type 'a t = {
    q: 'a Queue.t;
    mutex: Mutex.t;
  }

  let create () : _ t = { q = Queue.create (); mutex = Mutex.create () }

  let push (self : _ t) x : unit =
    Mutex.lock self.mutex;
    Queue.push x self.q;
    Mutex.unlock self.mutex

  let transfer (self : _ t) q : unit =
    Mutex.lock self.mutex;
    Queue.transfer self.q q;
    Mutex.unlock self.mutex
end

(** Callback list *)
type cbs =
  | Nil
  | Sub : 'a * 'b * (closed:bool -> 'a -> 'b -> unit) * cbs -> cbs

type timer_ev =
  | Timer : {
      deadline: int64;
      x: 'a;
      y: 'b;
      f: 'a -> 'b -> unit;
    }
      -> timer_ev

type fd_data = {
  fd: Unix.file_descr;
  mutable idx: int;
      (** Index in the poll buffer. Mutable because we might change it when we
          swap FDs to remove items. *)
  mutable r: cbs;
  mutable w: cbs;
}
(** Data associated to a given FD *)

let[@inline] fd_flags (self : fd_data) : Flags.t =
  let fl = ref Flags.empty in
  (if self.r != Nil then fl := Flags.(!fl + pollin));
  (if self.w != Nil then fl := Flags.(!fl + pollout));
  !fl

type queued_task =
  | Q_run_after of timer_ev
  | Q_on_readable :
      Unix.file_descr * 'a * 'b * (closed:bool -> 'a -> 'b -> unit)
      -> queued_task
  | Q_on_writable :
      Unix.file_descr * 'a * 'b * (closed:bool -> 'a -> 'b -> unit)
      -> queued_task
  | Q_clear
  | Q_close of Unix.file_descr

type st = {
  timer: timer_ev Heap.t;
  fds: fd_data Fd_tbl.t;
  poll: P.t;
  mutable len: int;  (** length of the active prefix of the [poll] buffer *)
  wakeup_rd: Unix.file_descr;
  wakeup_wr: Unix.file_descr;
  wakeup_triggered: bool Atomic.t;
      (** Make [wakeup_from_outside] idempotent within an iteration of [step] *)
  in_poll: bool Atomic.t;
      (** Are we currently inside a call to [poll], and in which thread? Useful
          for other threads to know whether to wake us up via the pipe *)
  mutable owner_thread: int;
      (** Thread allowed to perform operations on this poll instance. Starts at
          [-1]. *)
  queued_tasks: queued_task Sync_queue.t;
      (** While in [poll()], changes get queued, so we don't invalidate the poll
          buffer before the syscall returns *)
}

let[@inline] queue_task_ (self : st) t : unit =
  Sync_queue.push self.queued_tasks t

(** [true] if called from the owner thread *)
let[@inline] in_owner_thread (self : st) : bool =
  self.owner_thread != -1 && self.owner_thread == Thread.(id (self ()))

let[@inline] in_poll (self : st) : bool = Atomic.get self.in_poll
let[@inline] leq_timer (Timer a) (Timer b) = a.deadline <= b.deadline

let create_st () : st =
  let wakeup_rd, wakeup_wr = Unix.pipe () in
  (* reading end must be non blocking so it's not always immediately
    ready; writing end is blocking to make it simpler to wakeup from other
    threads *)
  Unix.set_nonblock wakeup_rd;
  let self =
    {
      timer = Heap.create ~leq:leq_timer ();
      fds = Fd_tbl.create 16;
      poll = P.create ();
      len = 0;
      wakeup_rd;
      wakeup_wr;
      wakeup_triggered = Atomic.make false;
      in_poll = Atomic.make false;
      owner_thread = -1;
      queued_tasks = Sync_queue.create ();
    }
  in

  (* always watch for the pipe being readable *)
  P.set_index self.poll 0 self.wakeup_rd Flags.pollin;
  self.len <- 1;

  self

let max_fds (self : st) : int = P.maxfds self.poll

let[@inline never] wakeup_real_ (self : st) : unit =
  let@ _sp =
    Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.wakeup-from-outside"
  in
  let b = Bytes.make 1 '!' in
  ignore (Unix.write self.wakeup_wr b 0 1 : int)

let[@inline] wakeup_ (self : st) : unit =
  if not (Atomic.exchange self.wakeup_triggered true) then wakeup_real_ self

let wakeup_from_outside (self : st) : unit =
  let already_awake =
    (* to avoid race conditions we only take the shortcut if
        this is called from the owner thread *)
    in_owner_thread self && not (Atomic.get self.in_poll)
  in
  if not already_awake then wakeup_ self

let rec perform_cbs ~closed = function
  | Nil -> ()
  | Sub (x, y, f, tail) ->
    f ~closed x y;
    perform_cbs ~closed tail

(** Change the event loop right now. This must be called only from the owner
    thread and outside of [poll]. *)
module Run_now_ = struct
  let rec perform_cbs_closed ~closed = function
    | Nil -> ()
    | Sub (x, y, f, tail) ->
      f ~closed x y;
      perform_cbs_closed ~closed tail

  let clear_ (self : st) : unit =
    Heap.clear self.timer;
    Fd_tbl.clear self.fds;
    for i = 0 to P.maxfds self.poll - 1 do
      P.set_index self.poll i P.invalid_fd Flags.empty
    done;
    Atomic.set self.wakeup_triggered false;
    self.len <- 0;
    ()

  let get_fd_ (self : st) fd : fd_data =
    (* assert (in_owner_thread self && not (in_poll self)); *)
    match Fd_tbl.find self.fds fd with
    | per_fd -> per_fd
    | exception Not_found ->
      let idx =
        if self.len = P.maxfds self.poll then
          invalid_arg "No available slot in poll";
        let n = self.len in
        self.len <- self.len + 1;
        n
      in
      let per_fd = { idx; fd; r = Nil; w = Nil } in
      Fd_tbl.add self.fds fd per_fd;
      per_fd

  let remove_fd_ (self : st) (fd_data : fd_data) : unit =
    Fd_tbl.remove self.fds fd_data.fd;
    P.set_index self.poll fd_data.idx P.invalid_fd Flags.empty;

    (* assert (in_owner_thread self && not (in_poll self)); *)
    if fd_data.idx > 0 && fd_data.idx + 1 < self.len then (
      (* not the last element nor the first (pipe_rd), move the last element
        here to keep the buffer non sparse *)
      let last_fd = P.get_fd self.poll (self.len - 1) in
      assert (last_fd <> fd_data.fd);
      match Fd_tbl.find_opt self.fds last_fd with
      | None -> assert false
      | Some last_fd_data ->
        (* move the last FD to [idx] *)
        last_fd_data.idx <- fd_data.idx;
        P.set_index self.poll fd_data.idx last_fd (fd_flags last_fd_data)
    );

    self.len <- self.len - 1;
    ()

  let close_ (self : st) fd : unit =
    let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.close" in
    let r, w =
      match Fd_tbl.find self.fds fd with
      | fd_data ->
        remove_fd_ self fd_data;
        fd_data.r, fd_data.w
      | exception Not_found -> Nil, Nil
    in
    perform_cbs_closed ~closed:true r;
    perform_cbs_closed ~closed:true w;
    ()

  let on_readable_ self fd x y f : unit =
    let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.on-readable" in
    let fd_data = get_fd_ self fd in
    fd_data.r <- Sub (x, y, f, fd_data.r);
    P.set_index self.poll fd_data.idx fd (fd_flags fd_data)

  let on_writable_ self fd x y f : unit =
    let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.on-writable" in
    let fd_data = get_fd_ self fd in
    fd_data.w <- Sub (x, y, f, fd_data.w);
    P.set_index self.poll fd_data.idx fd (fd_flags fd_data)

  let run_after_s_ self ev : unit = Heap.insert self.timer ev

  let perform_task_ self (t : queued_task) : unit =
    match t with
    | Q_run_after t -> run_after_s_ self t
    | Q_on_readable (fd, x, y, f) -> on_readable_ self fd x y f
    | Q_on_writable (fd, x, y, f) -> on_writable_ self fd x y f
    | Q_clear -> clear_ self
    | Q_close fd -> close_ self fd
end

let clear (self : st) =
  if in_owner_thread self && not (in_poll self) then
    Run_now_.clear_ self
  else (
    queue_task_ self @@ Q_clear;
    wakeup_from_outside self
  )

let close (self : st) fd : unit =
  if in_owner_thread self && not (in_poll self) then
    Run_now_.close_ self fd
  else (
    queue_task_ self @@ Q_close fd;
    wakeup_from_outside self
  )

let on_readable self fd x y f : unit =
  if in_owner_thread self && not (in_poll self) then
    Run_now_.on_readable_ self fd x y f
  else (
    queue_task_ self @@ Q_on_readable (fd, x, y, f);
    wakeup_from_outside self
  )

let on_writable self fd x y f : unit =
  if in_owner_thread self && not (in_poll self) then
    Run_now_.on_writable_ self fd x y f
  else (
    queue_task_ self @@ Q_on_writable (fd, x, y, f);
    wakeup_from_outside self
  )

let run_after_s self (time : float) x y f : unit =
  let deadline = Int64.add (now_ns ()) (ns_of_s time) in
  let ev = Timer { deadline; x; y; f } in
  if in_owner_thread self && not (in_poll self) then
    Run_now_.run_after_s_ self ev
  else (
    queue_task_ self @@ Q_run_after ev;
    wakeup_from_outside self
  )

let next_deadline_ (self : st) : int64 option =
  match Heap.peek_min_exn self.timer with
  | exception Heap.Empty -> None
  | Timer t -> Some t.deadline

let step (self : st) : unit =
  let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.posix.step" in

  self.owner_thread <- Thread.(id (self ()));
  let now = now_ns () in
  let timeout_ns : int64 =
    match next_deadline_ self with
    | None -> 30_000_000_000L
    | Some d -> Int64.max 0L (Int64.sub d now)
  in

  (* run timers *)
  while
    if Heap.is_empty self.timer then
      false
    else (
      let (Timer t) = Heap.peek_min_exn self.timer in
      if t.deadline <= now then (
        ignore (Heap.pop_min_exn self.timer : timer_ev);
        t.f t.x t.y;
        true
      ) else
        false
    )
  do
    ()
  done;

  (* process all queued tasks.

    NOTE: race condition: if another thread queues tasks after we do
    the transfer, it will call [wakeup_from_outside] and make the pipe_rd FD
    readable. So as soon as we call [poll], it will return and we will find
    the queued tasks waiting for us. *)
  let local_q = Queue.create () in
  Sync_queue.transfer self.queued_tasks local_q;
  while not (Queue.is_empty local_q) do
    let t = Queue.pop local_q in
    Run_now_.perform_task_ self t
  done;

  Atomic.set self.in_poll true;

  (* enter [poll] *)
  let num_ready_fds =
    let@ _sp =
      Trace_.with_span ~__FILE__ ~__LINE__ "poll" ~data:(fun () ->
          [ "timeout", `Float (ns_to_s timeout_ns); "len", `Int self.len ])
    in
    P.ppoll_or_poll self.poll self.len (Nanoseconds timeout_ns)
  in

  Atomic.set self.in_poll false;

  (* drain notification pipe *)
  if Atomic.exchange self.wakeup_triggered false then (
    let b1 = Bytes.create 8 in
    while try Unix.read self.wakeup_rd b1 0 8 > 0 with _ -> false do
      ()
    done
  );

  (* call callbacks *)
  P.iter_ready self.poll num_ready_fds (fun _idx fd flags ->
      if fd <> self.wakeup_rd then (
        let fd_data =
          try Fd_tbl.find self.fds fd with Not_found -> assert false
        in

        if Flags.mem Flags.pollin flags then (
          let r = fd_data.r in
          fd_data.r <- Nil;
          perform_cbs ~closed:false r
        );
        if Flags.mem Flags.pollout flags then (
          let w = fd_data.w in
          fd_data.w <- Nil;
          perform_cbs ~closed:false w
        );

        if Flags.empty = fd_flags fd_data then Run_now_.remove_fd_ self fd_data
      ));

  ()

let ops : st Nanoev.Impl.ops =
  {
    step;
    close;
    on_readable;
    on_writable;
    run_after_s;
    max_fds;
    wakeup_from_outside;
    clear;
  }

include Nanoev

let create () : t = Impl.build ops (create_st ())
