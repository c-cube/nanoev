module EV = Nanoev_picos
include Ezcurl_core

open struct
  (* TODO: have a thread-local multi handle? *)
  let multi : Curl.Multi.mt option Atomic.t = Atomic.make None
  let lock = Mutex.create ()

  let get_mt () =
    match Atomic.get multi with
    | Some mt -> mt
end

include Ezcurl_core.Make (struct
  type 'a t = 'a

  let[@inline] return x = x
  let fail = raise
  let ( >>= ) = ( |> )
  let ( >|= ) = ( |> )
  let perform (client : Curl.t) : Curl.curlCode = Curl.Multi.p
end)
