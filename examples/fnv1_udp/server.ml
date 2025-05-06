module Net = Nanoev_picos.Net
module Trace = Trace_core

let ( let@ ) = ( @@ )
let port = ref 9427
let j = ref (Domain.recommended_domain_count ())

(** FNV hashing
    https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
*)
module FNV = struct
  let fnv_offset_basis = 0xcbf29ce484222325L
  let fnv_prime = 0x100000001b3L

  let bytes ?len (x : bytes) : int64 =
    let h = ref fnv_offset_basis in
    let len = Option.value ~default:(Bytes.length x) len in
    for i = 0 to len - 1 do
      (h := Int64.(mul !h fnv_prime));
      let byte = Char.code (Bytes.unsafe_get x i) in
      h := Int64.(logxor !h (of_int byte))
    done;
    Int64.(logand !h max_int)

  let string (x : string) = bytes (Bytes.unsafe_of_string x)
end

let n_requests = Atomic.make 0
let n_replies = Atomic.make 0

let main () : unit =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.set_nonblock sock;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, !port));

  let@ pool = Moonpool.Ws_pool.with_ ~num_threads:!j () in

  if Trace.enabled () then
    ignore
      (Thread.create
         (fun () ->
           while true do
             Trace.counter_int "n-requests" (Atomic.get n_requests);
             Trace.counter_int "n-replies" (Atomic.get n_replies);

             Thread.delay 0.2
           done)
         ()
        : Thread.t);

  let buf = Bytes.create 1024 in
  while true do
    let n, remoteaddr = Net.recvfrom sock buf 0 (Bytes.length buf) ~flags:[] in
    Atomic.incr n_requests;
    let buf2 = Bytes.sub buf 0 n in
    Moonpool.run_async pool (fun () ->
        let h = FNV.bytes buf2 in
        let buf_ref = Bytes.create 8 in
        Bytes.set_int64_le buf_ref 0 h;
        ignore (Net.sendto sock ~addr:remoteaddr buf_ref 0 8 ~flags:[] : int));
    Atomic.incr n_replies
  done

let () =
  let@ () = Trace_tef.with_setup () in
  Arg.parse
    (Arg.align
       [
         "-p", Arg.Set_int port, " set port";
         "-j", Arg.Set_int j, " thread pool size";
       ])
    ignore "";
  Nanoev_picos.Background_thread.setup @@ Nanoev_posix.create ();
  let@ _run = Moonpool_fib.main in
  main ()
