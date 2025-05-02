open Common_

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

module Raw = struct
  let retry_read = retry_read_
  let retry_write = retry_write_
end
