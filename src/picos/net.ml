let[@inline] send sock buf i len ~flags : int =
  Base.Raw.retry_write sock (fun () -> Unix.send sock buf i len flags)

let[@inline] sendto sock ~addr ~flags buf i len : int =
  Base.Raw.retry_write sock (fun () -> Unix.sendto sock buf i len flags addr)

let[@inline] recv sock buf i len ~flags : int =
  Base.Raw.retry_read sock (fun () -> Unix.recv sock buf i len flags)

let[@inline] recvfrom sock buf i len ~flags : int * Unix.sockaddr =
  Base.Raw.retry_read sock (fun () -> Unix.recvfrom sock buf i len flags)
