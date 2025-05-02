module Trace = Trace_core
module F = Moonpool_fib
module IO = Nanoev_picos
module Sem = Picos_std_sync.Semaphore.Counting

[@@@ocaml.alert "-deprecated"]

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
let pf = Printf.printf
let verbose = ref false
let reset_line = "\x1b[2K\r"
let n_loops_per_task = 100

let main ~runner:_ ~port ~unix_sock ~n ~n_conn () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  pf "connect on %s n=%d n_conn=%d\n%!"
    (if unix_sock = "" then
       spf "localhost:%d" port
     else
       spf "unix:%S" unix_sock)
    n n_conn;

  let addr =
    if unix_sock = "" then
      Unix.ADDR_INET (Unix.inet_addr_loopback, port)
    else
      Unix.ADDR_UNIX unix_sock
  in

  Printf.printf "connecting to port %d\n%!" port;

  let all_done = Atomic.make false in
  let n_queries = Atomic.make 0 in

  (* limit simultaneous number of connections *)
  let sem = Sem.make n_conn in
  let n_active_conns = Atomic.make 0 in

  let progress_loop () =
    while not (Atomic.get all_done) do
      let n_queries = Atomic.get n_queries in
      let n_conns = Atomic.get n_active_conns in

      (* progress *)
      Printf.printf "%sdone %d queries, %d active connections%!" reset_line
        n_queries n_conns;

      Trace.counter_int ~level:Info "n-conns" n_conns;
      Trace.counter_int ~level:Info "n-queries" n_queries;
      let gc = Gc.quick_stat () in
      Trace.counter_int ~level:Info "gc.major" gc.major_collections;
      Trace.counter_int ~level:Info "gc.minor" gc.minor_collections;
      Trace.counter_int ~level:Info "gc.heap-size" (gc.heap_words * 64);

      Thread.delay 0.2
    done
  in

  ignore (Thread.create progress_loop () : Thread.t);

  let run_task () =
    let _task_sp =
      Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "run-task"
    in
    Sem.acquire sem;
    ( IO.Net_client.with_connect addr @@ fun ic oc ->
      Atomic.incr n_active_conns;
      let buf = Bytes.create 32 in

      for _j = 1 to n_loops_per_task do
        (*let _sp =
            Trace.enter_manual_sub_span ~parent:_task_sp ~__FILE__ ~__LINE__
              "write.loop" ~data:(fun () -> [ "iter", `Int _j ])
          in*)
        Atomic.incr n_queries;

        Iostream.Out.output_string oc "hello";
        Iostream.Out_buf.flush oc;

        (* read back what we wrote *)
        Iostream.In.really_input ic buf 0 (String.length "hello");
        (* Trace.exit_manual_span _sp; *)
        F.yield ()
      done;

      Atomic.decr n_active_conns;
      Sem.release sem );

    Trace.exit_manual_span _task_sp
  in

  let t_start = Mtime_clock.now () in

  (* start the first [n_conn] tasks *)
  let fibers = List.init (n * n_conn) (fun _ -> F.spawn run_task) in
  List.iter F.await fibers;
  Atomic.set all_done true;

  let t_stop = Mtime_clock.now () in
  let elapsed_s =
    (Mtime.span t_start t_stop |> Mtime.Span.to_uint64_ns |> Int64.to_float)
    *. 1e-9
  in

  (* exit when [fut_exit] is resolved *)
  Printf.printf
    "%sdone with main (time=%.4fs, n queries=%d, expect=%d, %.3f req/s)\n%!"
    reset_line elapsed_s (Atomic.get n_queries)
    (n * n_conn * n_loops_per_task)
    (float (Atomic.get n_queries) /. elapsed_s)

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_current_level Info;
  Trace.set_thread_name "main";

  let port = ref 1234 in
  let unix_sock = ref "" in
  let n = ref 1000 in
  let n_conn = ref 20 in
  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "-v", Arg.Set verbose, " verbose";
      "-n", Arg.Set_int n, " number of iterations";
      "--unix", Arg.Set_string unix_sock, " unix socket";
      "--n-conn", Arg.Set_int n_conn, " number of simultaneous connections";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo_client";

  let@ () =
    Nanoev_picos.Background_thread.with_setup (Nanoev_posix.create ())
  in
  F.main @@ fun runner ->
  main ~runner ~port:!port ~unix_sock:!unix_sock ~n:!n ~n_conn:!n_conn ()
