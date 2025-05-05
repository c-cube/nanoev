open Common_

let get_loop_exn_ : unit -> Nanoev.t = Global_ev.get_nanoev_exn
let[@inline] check_fiber_ () : unit = Picos.Fiber.(check @@ current ())

let[@inline] unwrap_ = function
  | None -> ()
  | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt

let[@inline] on_readable_ fd x y f : unit =
  let ev = get_loop_exn_ () in
  Nanoev.on_readable ev fd x y f

let[@unroll 1] rec retry_read_ fd f =
  check_fiber_ ();
  match f () with
  | res -> res
  | exception
      Unix.Unix_error
        ( ( Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR | Unix.EINPROGRESS
          | Unix.ECONNRESET ),
          _,
          _ ) ->
    (* Trace_.message "read must wait"; *)
    let trigger = Picos.Trigger.create () in
    let closed_r = ref false in
    on_readable_ fd trigger closed_r (fun ~closed trigger closed_r ->
        closed_r := closed;
        Picos.Trigger.signal trigger);
    Picos.Trigger.await trigger |> unwrap_;
    if !closed_r then raise Closed;
    retry_read_ fd f

let[@inline] on_writable_ fd x y f : unit =
  let ev = get_loop_exn_ () in
  Nanoev.on_writable ev fd x y f

let[@unroll 1] rec retry_write_ fd f =
  check_fiber_ ();
  match f () with
  | res -> res
  | exception
      Unix.Unix_error
        ( ( Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR | Unix.EINPROGRESS
          | Unix.ECONNRESET ),
          _,
          _ ) ->
    (* Trace_.message "write must wait"; *)
    let trigger = Picos.Trigger.create () in
    let closed_r = ref false in
    on_writable_ fd trigger closed_r (fun ~closed trigger closed_r ->
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

let write_once fd buf i len : int =
  try
    retry_write_ fd (fun () ->
        (* Trace_.message "write"; *)
        Unix.write fd buf i len)
  with Closed -> 0

let rec write fd buf i len =
  if len > 0 then (
    let n = write_once fd buf i len in
    if n < len then write fd buf (i + n) (len - n)
  )

let connect fd addr = retry_write_ fd (fun () -> Unix.connect fd addr)

let[@inline] max_fds () =
  Option.fold ~none:1024 ~some:Nanoev.max_fds @@ Global_ev.get_nanoev ()

let run_after_s_ t x y f : unit =
  let ev = get_loop_exn_ () in
  Nanoev.run_after_s ev t x y f

let sleep t =
  if t > 0. then (
    let trigger = Picos.Trigger.create () in
    run_after_s_ t trigger () (fun trigger () -> Picos.Trigger.signal trigger);
    Picos.Trigger.await trigger |> unwrap_;
    check_fiber_ ()
  )

module Raw = struct
  let run_after_s = run_after_s_
  let on_readable = on_readable_
  let on_writable = on_writable_
  let retry_read = retry_read_
  let retry_write = retry_write_
end
