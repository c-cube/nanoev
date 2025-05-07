module Net = Nanoev_picos.Net
module Trace = Trace_core

let ( let@ ) = ( @@ )
let port = ref 9427
let j = ref (Domain.recommended_domain_count ())
let n_iters = ref 200
let n_tasks = ref 100
let reset_line = "\x1b[2K\r"

(** Message we send *)
let txt = String.init 100 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

let main () : unit =
  let n_tasks_done = Atomic.make 0 in
  let n_sent = Atomic.make 0 in
  let n_recv = Atomic.make 0 in

  let remoteaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, !port) in

  (let@ pool = Moonpool.Ws_pool.with_ ~num_threads:!j () in

   ignore
     (Thread.create
        (fun () ->
          while true do
            Printf.printf "%stasks done: %d sent: %d recv: %d%!" reset_line
              (Atomic.get n_tasks_done) (Atomic.get n_sent) (Atomic.get n_recv);

            Trace.counter_int "task-done" (Atomic.get n_tasks_done);
            Trace.counter_int "sent" (Atomic.get n_sent);
            Trace.counter_int "recv" (Atomic.get n_recv);

            Thread.delay 0.2
          done)
        ()
       : Thread.t);

   let task () =
     let[@alert "-deprecated"] _sp =
       Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "task.run"
     in

     let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
     Unix.set_nonblock sock;

     let buf_read = Bytes.create 9 in
     for _i = 1 to !n_iters do
       let ack = Atomic.make false in

       (* response might get lost, so we send a message until
          we get a response *)
       let fut =
         Moonpool.spawn ~on:pool @@ fun () ->
         while not (Atomic.get ack) do
           let _n =
             Net.sendto sock ~addr:remoteaddr ~flags:[]
               (Bytes.unsafe_of_string txt)
               0 (String.length txt)
           in
           Atomic.incr n_sent;
           assert (_n = 100);
           (* retry later *)
           Nanoev_picos.sleep 0.001
         done
       in

       if Random.float 1. > 0.992 then
         Trace.counter_int "n-tasks" (Moonpool.Runner.num_tasks pool);

       Picos.Fiber.(yield ());

       (* receive a response *)
       let _n, _remote = Net.recvfrom sock buf_read 0 9 ~flags:[] in
       assert (_n = 8);
       Atomic.incr n_recv;
       Atomic.set ack true;

       (* cleanup *)
       Moonpool.await fut
     done;

     Unix.close sock;
     Atomic.incr n_tasks_done;
     Trace.exit_manual_span _sp
   in

   let tasks = Dynarray.create () in
   for _i = 1 to !n_tasks do
     Dynarray.add_last tasks (Moonpool.spawn ~on:pool task)
   done;
   Dynarray.iter Moonpool.Fut.wait_block_exn tasks);
  Printf.printf "%sall done\n%!" reset_line

let () =
  let@ () = Trace_tef.with_setup () in
  Arg.parse
    (Arg.align
       [
         "-p", Arg.Set_int port, " set port";
         "-j", Arg.Set_int j, " thread pool size";
         "--n-tasks", Arg.Set_int n_tasks, " number of tasks";
         "--n-iters", Arg.Set_int n_iters, " number of iterations per task";
       ])
    ignore "";
  Nanoev_picos.Background_thread.setup @@ Nanoev_posix.create ();
  let@ _run = Moonpool_fib.main in
  main ()
