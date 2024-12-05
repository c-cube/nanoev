type ('st, 'action, 'input, 'a, 'err) t = {
  inputs: 'input Queue.t;
  actions: 'action Queue.t;
  mutable next: ('st, 'action, 'input, 'a, 'err) next;
}

and ('st, 'action, 'input, 'a, 'err) next =
  | N_return of 'a
  | N_fail of 'err
  | N_run of ('st -> ('st, 'action, 'input, 'a, 'err) transition)
  | N_await of ('st -> 'input -> ('st, 'action, 'input, 'a, 'err) transition)

and ('st, 'action, 'input, 'a, 'err) transition =
  | T_return of 'a
  | T_fail of 'err
  | T_perform of 'action * ('st, 'action, 'input, 'a, 'err) next
  | T_await of ('st -> 'input -> ('st, 'action, 'input, 'a, 'err) transition)

type ('a, 'err) res =
  | Working
  | Return of 'a
  | Fail of 'err

(**/*)

let return x : _ transition = T_return x
let fail err : _ transition = T_fail err
let perform act f : _ transition = T_perform (act, N_run f)
let perform_and_await act f : _ transition = T_perform (act, N_await f)
let await f : _ transition = T_await f

(**/*)

let create run : _ t =
  { actions = Queue.create (); inputs = Queue.create (); next = N_run run }

let create_rec run : _ t =
  let rec loop st = run st ~recurse:loop in
  create loop

let pop_action (self : _ t) : _ option = Queue.take_opt self.actions

(** Add external input to the state machine *)
let add_input self i : unit = Queue.push i self.inputs

(** Perform some work *)
let work self st : _ res =
  let rec loop () =
    match self.next with
    | N_return x -> Return x
    | N_fail err -> Fail err
    | N_run f -> transition (f st)
    | N_await f -> run_on_next_input f
  and transition tr =
    match tr with
    | T_return x ->
      self.next <- N_return x;
      Return x
    | T_fail err ->
      self.next <- N_fail err;
      Fail err
    | T_perform (a, n) ->
      Queue.push a self.actions;
      self.next <- n;
      loop ()
    | T_await f -> run_on_next_input f
  and run_on_next_input f =
    match Queue.take self.inputs with
    | exception Queue.Empty ->
      self.next <- N_await f;
      Working
    | input -> transition (f st input)
  in
  loop ()
