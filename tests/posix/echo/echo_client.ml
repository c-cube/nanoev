module Trace = Trace_core
module F = Moonpool_fib
module IO = Nanoev_picos

[@@@ocaml.alert "-deprecated"]

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
let pf = Printf.printf
let verbose = ref false

let main ~port ~unix_sock ~n ~n_conn () =
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

  let remaining = Atomic.make n in
  let all_done = Atomic.make 0 in

  Printf.printf "connecting to port %d\n%!" port;

  let rec run_task () =
    let n = Atomic.fetch_and_add remaining (-1) in
    let _task_sp =
      Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "run-task"
        ~data:(fun () -> [ "n", `Int n ])
    in
    if n > 0 then (
      ( (* let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "connect.client" in *)
        IO.Net_client.with_connect addr
      @@ fun ic oc ->
        let buf = Bytes.create 32 in

        for _j = 1 to 100 do
          let _sp =
            Trace.enter_manual_sub_span ~parent:(Trace.ctx_of_span _task_sp)
              ~__FILE__ ~__LINE__ "write.loop" ~data:(fun () ->
                [ "iter", `Int _j ])
          in
          Iostream.Out.output_string oc "hello";
          Iostream.Out_buf.flush oc;

          (* read back what we wrote *)
          Iostream.In.really_input ic buf 0 (String.length "hello");
          Trace.exit_manual_span _sp;
          F.yield ()
        done );

      (* run another task *)
      F.spawn_ignore run_task
    ) else (
      (* if we're the last to exit, resolve the promise *)
      let n_already_done = Atomic.fetch_and_add all_done 1 in
      if n_already_done = n_conn - 1 then Printf.printf "all done\n%!"
    );
    Trace.exit_manual_span _task_sp
  in

  (* start the first [n_conn] tasks *)
  let fibers = List.init n_conn (fun _ -> F.spawn run_task) in
  List.iter F.await fibers;

  (* exit when [fut_exit] is resolved *)
  Printf.printf "done with main\n%!"

let () =
  let@ () = Trace_tef.with_setup () in
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
  F.main @@ fun _runner ->
  main ~port:!port ~unix_sock:!unix_sock ~n:!n ~n_conn:!n_conn ()
