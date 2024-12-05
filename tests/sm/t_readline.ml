module SM = Nanoev_sm

let spf = Printf.sprintf
let pf = Printf.printf

module St = struct
  type t = {
    mutable bs: bytes;
    mutable len: int;
  }

  let create () : t = { bs = Bytes.create 32; len = 0 }

  let add_bytes (self : t) buf i len : unit =
    if Bytes.length self.bs < len + self.len then (
      let new_bs = Bytes.create (max (self.len * 2) (self.len + len + 2)) in
      Bytes.blit self.bs 0 new_bs 0 self.len;
      self.bs <- new_bs
    );
    assert (self.len + len <= Bytes.length self.bs);
    Bytes.blit buf i self.bs self.len len;
    self.len <- self.len + len

  let shift_left (self : t) n =
    if n = self.len then
      self.len <- 0
    else (
      Bytes.blit self.bs n self.bs 0 (self.len - n);
      self.len <- self.len - n
    )
end

open struct
  exception Found of int
end

type input =
  [ `End_of_input
  | `Input of bytes * int * int
  ]

let readline () : ([ `Yield of string ], input, unit, SM.void) SM.t =
  let self = St.create () in
  SM.create_rec @@ fun ~aq ~recurse ->
  match
    for i = 0 to self.len - 1 do
      if Bytes.get self.bs i = '\n' then raise_notrace (Found i)
    done
  with
  | () ->
    SM.await @@ fun input ->
    (match input with
    | `End_of_input when self.len = 0 -> SM.return ()
    | `End_of_input ->
      let line = Bytes.sub_string self.bs 0 self.len in
      St.shift_left self self.len;
      SM.perform ~aq (`Yield line);
      SM.return ()
    | `Input (buf, i, len) ->
      St.add_bytes self buf i len;
      recurse ())
  | exception Found i ->
    let line = Bytes.sub_string self.bs 0 i in
    St.shift_left self (i + 1);
    SM.perform ~aq (`Yield line);
    recurse ()

let test (inputs : string list) =
  pf "## test on [%s]\n%!" (String.concat ";" @@ List.map (spf "%S") inputs);

  let inputs = Queue.of_seq @@ List.to_seq inputs in
  let rd = readline () in

  let work () =
    (match SM.work rd with
    | Ok () -> pf "done\n"
    | Error _ -> .
    | Working -> ());
    SM.pop_actions rd (function `Yield line -> pf "yielded line %S\n%!" line)
  in

  work ();
  while not (Queue.is_empty inputs) do
    let s = Queue.pop inputs in
    pf "feed input %S\n%!" s;
    SM.add_input rd (`Input (Bytes.of_string s, 0, String.length s));
    work ()
  done;
  SM.add_input rd `End_of_input;
  work ();

  pf "## end test\n"

let () =
  test [ "hello\nwor"; "ld\n\n123\n" ];
  test
    [
      "a very very long";
      " line over here, wow, ";
      "damn!\noh well\n";
      "\nanother ";
      "one?\n";
      "all done\n";
    ];
  ()
