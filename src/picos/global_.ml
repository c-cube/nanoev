open Common_

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
