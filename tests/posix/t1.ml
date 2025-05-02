module E = Nanoev_posix

let mkpipe () : Unix.file_descr * Unix.file_descr =
  let f1, f2 = Unix.pipe () in
  Unix.set_nonblock f1;
  Unix.set_nonblock f2;
  f1, f2

let loop (e : E.t) =
  while true do
    E.step e
  done

let () =
  let ev = E.create () in
  ignore (Thread.create loop ev : Thread.t);
  let rd, wr = mkpipe () in
  E.on_readable ev rd () () (fun ~closed () () ->
      if closed then
        print_endline "closed!"
      else
        print_endline "can read");
  Thread.delay 0.05;
  print_endline "writing";
  ignore
    (Unix.write wr (Bytes.unsafe_of_string "hello") 0 (String.length "hello")
      : int);
  Thread.delay 0.1;
  print_endline "done writing"
