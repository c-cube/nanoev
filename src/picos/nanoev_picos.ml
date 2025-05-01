open struct
  module Trace_ = Nanoev.Trace_

  let ( let@ ) = ( @@ )
end

exception Closed = Nanoev.Closed

module Global_ = struct
  type st =
    | None
    | Some of {
        active: bool Atomic.t;
        nanoev: Nanoev.t;
        th: Thread.t;
      }

  let st : st Atomic.t = Atomic.make None
  let lock = Mutex.create ()

  let with_lock lock f =
    Mutex.lock lock;
    match f () with
    | exception e ->
      Mutex.unlock lock;
      raise e
    | x ->
      Mutex.unlock lock;
      x

  let bg_thread_ ~active ~evloop () : unit =
    Trace_.set_thread_name "nanoev.picos.bg-thread";
    while Atomic.get active do
      Nanoev.step evloop
    done

  let[@inline] has_bg_thread () = Atomic.get st <> None

  let setup_bg_thread (ev : Nanoev.t) : unit =
    let@ () = with_lock lock in
    (* shutdown existing thread, if any *)
    (match Atomic.get st with
    | Some st ->
      Atomic.set st.active false;
      Nanoev.wakeup_from_outside st.nanoev;
      Thread.join st.th
    | None -> ());

    (* start new bg thread *)
    let active = Atomic.make true in
    Atomic.set st
    @@ Some
         {
           active;
           nanoev = ev;
           th = Thread.create (bg_thread_ ~active ~evloop:ev) ();
         }

  let shutdown_bg_thread () =
    let@ () = with_lock lock in
    match Atomic.exchange st None with
    | None -> ()
    | Some st ->
      Atomic.set st.active false;
      Nanoev.wakeup_from_outside st.nanoev;
      Thread.join st.th
end

module Background_thread = struct
  let is_setup = Global_.has_bg_thread
  let setup = Global_.setup_bg_thread
  let shutdown = Global_.shutdown_bg_thread

  let with_setup ev f =
    setup ev;
    Fun.protect ~finally:shutdown f
end

let[@inline] get_loop_exn_ () : Nanoev.t =
  match Atomic.get Global_.st with
  | None -> failwith "No nanoev loop installed."
  | Some st -> st.nanoev

let[@inline] unwrap_ = function
  | None -> ()
  | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt

let[@unroll 1] rec retry_read_ fd f =
  match f () with
  | res -> res
  | exception
      Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
    (* Trace_.message "read must wait"; *)
    let trigger = Picos.Trigger.create () in
    let closed_r = ref false in
    let ev = get_loop_exn_ () in
    Nanoev.on_readable ev fd trigger closed_r (fun ~closed trigger closed_r ->
        closed_r := closed;
        Picos.Trigger.signal trigger);
    Picos.Trigger.await trigger |> unwrap_;
    if !closed_r then raise Closed;
    retry_read_ fd f

let[@unroll 1] rec retry_write_ fd f =
  match f () with
  | res -> res
  | exception
      Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
    (* Trace_.message "write must wait"; *)
    let ev = get_loop_exn_ () in
    let trigger = Picos.Trigger.create () in
    let closed_r = ref false in
    Nanoev.on_writable ev fd trigger closed_r (fun ~closed trigger closed_r ->
        closed_r := closed;
        Picos.Trigger.signal trigger);
    Picos.Trigger.await trigger |> unwrap_;
    if !closed_r then raise Closed;
    retry_write_ fd f

let read fd buf i len : int =
  try
    retry_read_ fd (fun () ->
        (* Trace_.message "read"; *)
        Unix.read fd buf i len)
  with Closed -> 0

let close fd =
  Unix.close fd;
  let ev = get_loop_exn_ () in
  Nanoev.close ev fd

let accept fd =
  try
    retry_read_ fd (fun () ->
        (* Trace_.message "accept"; *)
        Unix.accept fd)
  with Unix.Unix_error ((Unix.ESHUTDOWN | Unix.ECONNABORTED), _, _) ->
    raise Closed

let write fd buf i len : int =
  try
    retry_write_ fd (fun () ->
        (* Trace_.message "write"; *)
        Unix.write fd buf i len)
  with Closed -> 0

let connect fd addr = retry_write_ fd (fun () -> Unix.connect fd addr)

let[@inline] max_fds () =
  match Atomic.get Global_.st with
  | None -> 1024
  | Some st -> Nanoev.max_fds st.nanoev

let sleep t =
  if t > 0. then (
    let ev = get_loop_exn_ () in
    let trigger = Picos.Trigger.create () in
    Nanoev.run_after_s ev t trigger () (fun trigger () ->
        Picos.Trigger.signal trigger);
    Picos.Trigger.await trigger |> unwrap_
  )
