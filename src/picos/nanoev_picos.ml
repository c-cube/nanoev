open struct
  module Trace_ = Nanoev.Trace_

  let ( let@ ) = ( @@ )
end

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
end

let has_bg_thread = Global_.has_bg_thread
let setup_bg_thread = Global_.setup_bg_thread

let[@inline] get_loop_exn_ () : Nanoev.t =
  match Atomic.get Global_.st with
  | None -> failwith "No nanoev loop installed."
  | Some st -> st.nanoev

let[@inline] unwrap_ = function
  | None -> ()
  | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt

let retry_read_ fd f =
  let ev = get_loop_exn_ () in
  let[@unroll 1] rec loop () =
    match f () with
    | res -> res
    | exception
        Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
      Trace_.message "read must wait";
      let trigger = Picos.Trigger.create () in
      Nanoev.on_readable ev fd trigger () (fun trigger () ->
          Picos.Trigger.signal trigger);
      Picos.Trigger.await trigger |> unwrap_;
      loop ()
  in
  loop ()

let retry_write_ fd f =
  let ev = get_loop_exn_ () in
  let rec loop () =
    match f () with
    | res -> res
    | exception
        Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
      Trace_.message "write must wait";
      let trigger = Picos.Trigger.create () in
      Nanoev.on_writable ev fd trigger () (fun trigger () ->
          Picos.Trigger.signal trigger);
      Picos.Trigger.await trigger |> unwrap_;
      loop ()
  in
  loop ()

let read fd buf i len : int =
  retry_read_ fd (fun () ->
      Trace_.message "read";
      Unix.read fd buf i len)

let accept fd =
  retry_read_ fd (fun () ->
      Trace_.message "accept";
      Unix.accept fd)

let write fd buf i len : int =
  retry_write_ fd (fun () ->
      Trace_.message "write";
      Unix.write fd buf i len)

let connect fd addr = retry_write_ fd (fun () -> Unix.connect fd addr)

let sleep t =
  if t > 0. then (
    let ev = get_loop_exn_ () in
    let trigger = Picos.Trigger.create () in
    Nanoev.run_after_s ev t trigger () (fun trigger () ->
        Picos.Trigger.signal trigger);
    Picos.Trigger.await trigger |> unwrap_
  )
