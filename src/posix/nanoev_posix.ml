open struct
  module Trace_ = Nanoev.Trace_

  let ( let@ ) = ( @@ )
  let now_ns : unit -> int64 = Mtime_clock.now_ns
end

module Fd_tbl = Hashtbl.Make (struct
  open Iomux.Util

  type t = Unix.file_descr

  let equal a b = Int.equal (fd_of_unix a) (fd_of_unix b)
  let hash a = Hashtbl.hash (fd_of_unix a)
end)

module P = Iomux.Poll
module Flags = P.Flags
open Iomux.Util

(* TODO: remove
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
end
*)

(** Callback list *)
type cbs =
  | Nil
  | Sub : 'a * 'b * (closed:bool -> 'a -> 'b -> unit) * cbs -> cbs

(** Single callback *)
type cb = Cb : 'a * 'b * (closed:bool -> 'a -> 'b -> unit) -> cb

let[@inline] cb_is_empty = function
  | Nil -> true
  | Sub _ -> false

type timer_ev =
  | Timer : {
      deadline: float;
      x: 'a;
      y: 'b;
      f: 'a -> 'b -> unit;
    }
      -> timer_ev

type per_fd = {
  fd: Unix.file_descr;
  mutable idx: int;
      (** Index in the buffer. Can change because we swap FDs sometimes to
          remove items. *)
  mutable r: cbs;
  mutable w: cbs;
}

let[@inline] per_fd_flags (self : per_fd) : Flags.t =
  let fl = ref Flags.empty in
  (if self.r <> Nil then fl := Flags.(!fl + pollin));
  (if self.w <> Nil then fl := Flags.(!fl + pollout));
  !fl

type queued_task =
  | Q_timer of timer_ev
  | Q_on_readable of Unix.file_descr * cb
  | Q_on_writable of Unix.file_descr * cb
  | Q_clear

type st = {
  timer: timer_ev Heap.t;
  fds: per_fd Fd_tbl.t;
  poll: P.t;
  mutable len: int;  (** length of the active prefix of the [poll] buffer *)
  wakeup_rd: Unix.file_descr;
  wakeup_wr: Unix.file_descr;
  wakeup_triggered: bool Atomic.t;
      (** Make [wakeup_from_outside] idempotent within an iteration of [step] *)
  in_poll: Thread.t option Atomic.t;
      (** Are we currently inside a call to [poll], and in which thread? Useful
          for other threads to know whether to wake us up via the pipe *)
  queued_tasks: queued_task Queue.t;
      (** While in [poll()], changes get queued, so we don't invalidate the poll
          buffer before the syscall returns *)
  lock: Mutex.t;
}

let rec perform_cbs ~closed = function
  | Nil -> ()
  | Sub (x, y, f, tail) ->
    f ~closed x y;
    perform_cbs ~closed tail

let rec perform_cbs_closed ~closed = function
  | Nil -> ()
  | Sub (x, y, f, tail) ->
    f ~closed x y;
    perform_cbs_closed ~closed tail

let[@inline] leq_timer (Timer a) (Timer b) = a.deadline <= b.deadline

let[@inline] with_lock_ (self : st) f =
  Mutex.lock self.lock;
  match f self with
  | exception e ->
    Mutex.unlock self.lock;
    raise e
  | res ->
    Mutex.unlock self.lock;
    res

let create_st () : st =
  let wakeup_rd, wakeup_wr = Unix.pipe () in
  Unix.set_nonblock wakeup_rd;
  {
    timer = Heap.create ~leq:leq_timer ();
    fds = Fd_tbl.create 16;
    poll = P.create ();
    len = 0;
    wakeup_rd;
    wakeup_wr;
    wakeup_triggered = Atomic.make false;
    in_poll = Atomic.make None;
    queued_tasks = Queue.create ();
    lock = Mutex.create ();
  }

let max_fds (self : st) : int = P.maxfds self.poll

let clear (self : st) =
  let@ self = with_lock_ self in
  Heap.clear self.timer;
  Fd_tbl.clear self.fds;
  for i = 0 to P.maxfds self.poll - 1 do
    P.set_index self.poll i P.invalid_fd Flags.empty
  done;
  self.len <- 0;
  ()

let wakeup_from_outside (self : st) : unit =
  if not (Atomic.exchange self.wakeup_triggered true) then
    let@ _sp =
      Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.wakeup-from-outside"
    in
    let b = Bytes.make 1 '!' in
    ignore (Unix.write self.wakeup_wr b 0 1 : int)

let get_fd_ (self : st) fd : per_fd =
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

let close_ (self : st) fd : unit =
  let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.close" in
  let r, w =
    let@ self = with_lock_ self in
    match Fd_tbl.find self.fds fd with
    | per_fd ->
      Fd_tbl.remove self.fds fd;

      (* not the last element, move the last element here *)
      if per_fd.idx + 1 < self.len then (
        let last_fd = P.get_fd self.poll (self.len - 1) in
        match Fd_tbl.find_opt self.fds last_fd with
        | None -> ()
        | Some last_per_fd ->
          last_per_fd.idx <- per_fd.idx;
          P.set_index self.poll per_fd.idx last_fd (per_fd_flags last_per_fd)
      );
      self.len <- self.len - 1;
      per_fd.r, per_fd.w
    | exception Not_found ->
      invalid_arg "File descriptor is not known to Nanoev"
  in

  (* call callbacks outside of the lock's critical section *)
  perform_cbs_closed ~closed:true r;
  perform_cbs_closed ~closed:true w;
  ()

let on_readable self fd x y f : unit =
  let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.on-readable" in
  let@ self = with_lock_ self in
  let per_fd = get_fd_ self fd in
  per_fd.r <- Sub (x, y, f, per_fd.r);
  (* FIXME: P.set_index *)
  self.sub_up_to_date <- false;
  if Atomic.get self.in_select then wakeup_from_outside self

let on_writable self fd x y f : unit =
  let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.on-writable" in
  let@ self = with_lock_ self in
  let per_fd = get_fd_ self fd in
  per_fd.w <- Sub (x, y, f, per_fd.w);
  (* FIXME: P.set_index *)
  self.sub_up_to_date <- false;
  if Atomic.get self.in_select then wakeup_from_outside self

let run_after_s self time x y f : unit =
  let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.run-after-s" in
  let@ self = with_lock_ self in
  let deadline = now_ () +. time in
  Heap.insert self.timer (Timer { deadline; x; y; f });
  if Atomic.get self.in_select then wakeup_from_outside self

let recompute_if_needed (self : st) =
  if not self.sub_up_to_date then (
    let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "recompute-if-needed" in
    self.sub_up_to_date <- true;
    self.sub_r <- [];
    self.sub_w <- [];
    Hashtbl.iter
      (fun fd per_fd ->
        if cb_is_empty per_fd.r && cb_is_empty per_fd.w then
          Hashtbl.remove self.fds fd;
        if not (cb_is_empty per_fd.r) then self.sub_r <- fd :: self.sub_r;
        if not (cb_is_empty per_fd.w) then self.sub_w <- fd :: self.sub_w)
      self.fds
  )

let next_deadline_ (self : st) : float option =
  match Heap.peek_min_exn self.timer with
  | exception Heap.Empty -> None
  | Timer t -> Some t.deadline

let step (self : st) : unit =
  let@ _sp = Trace_.with_span ~__FILE__ ~__LINE__ "nanoev.unix.step" in
  (* gather the subscriptions and timeout *)
  let timeout, sub_r, sub_w =
    let@ self = with_lock_ self in
    recompute_if_needed self;
    let timeout =
      match next_deadline_ self with
      | None -> 30.
      | Some d -> max 0. (d -. now_ ())
    in
    timeout, self.sub_r, self.sub_w
  in

  (* enter [select] *)
  Atomic.set self.in_select true;
  let r_reads, r_writes, _ =
    let@ _sp =
      Trace_.with_span ~__FILE__ ~__LINE__ "select" ~data:(fun () ->
          [
            "timeout", `Float timeout;
            "reads", `Int (List.length sub_r);
            "writes", `Int (List.length sub_w);
          ])
    in
    Unix.select (self.wakeup_rd :: sub_r) sub_w [] timeout
  in
  Atomic.set self.in_select false;

  (* drain pipe *)
  if Atomic.exchange self.wakeup_triggered false then (
    let b1 = Bytes.create 1 in
    while try Unix.read self.wakeup_rd b1 0 1 > 0 with _ -> false do
      ()
    done
  );

  (* gather the [per_fd] that are ready *)
  let ready_r = ref [] in
  let ready_w = ref [] in

  (* gather the [per_fd] that have updates *)
  (let@ self = with_lock_ self in
   if r_reads != [] || r_writes != [] then self.sub_up_to_date <- false;

   List.iter
     (fun fd ->
       if fd != self.wakeup_rd then (
         let per_fd = Hashtbl.find self.fds fd in
         ready_r := per_fd :: !ready_r
       ))
     r_reads;
   List.iter
     (fun fd ->
       let per_fd = Hashtbl.find self.fds fd in
       ready_w := per_fd :: !ready_w)
     r_writes);

  (* call callbacks *)
  List.iter
    (fun fd ->
      perform_cbs ~closed:false fd.r;
      fd.r <- Nil)
    !ready_r;
  List.iter
    (fun fd ->
      perform_cbs ~closed:false fd.w;
      fd.w <- Nil)
    !ready_w;

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
