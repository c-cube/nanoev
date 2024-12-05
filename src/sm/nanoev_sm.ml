type 'a iter = ('a -> unit) -> unit
type 'action action_queue = { aq: 'action Queue.t } [@@unboxed]
type void = |

type ('action, 'input, 'a, 'err) t = {
  inputs: 'input Queue.t;
  actions: 'action Queue.t;
  mutable next: ('action, 'input, 'a, 'err) next;
}

and ('action, 'input, 'a, 'err) next =
  | N_return of 'a
  | N_fail of 'err
  | N_run of (unit -> ('action, 'input, 'a, 'err) next)
  | N_await of ('input -> ('action, 'input, 'a, 'err) next)

type (+'a, +'err) res =
  | Working
  | Ok of 'a
  | Error of 'err

(**/*)

let return x : _ next = N_return x
let fail err : _ next = N_fail err
let perform ~(aq : _ action_queue) act : unit = Queue.push act aq.aq
let await f : _ next = N_await f

(**/*)

let create run : _ t =
  let actions = Queue.create () in
  let run () = run ~aq:{ aq = actions } () in
  { actions; inputs = Queue.create (); next = N_run run }

let create_rec run : _ t =
  let actions = Queue.create () in
  let rec loop () = run ~aq:{ aq = actions } ~recurse:loop in
  { actions; inputs = Queue.create (); next = N_run loop }

let pop_action (self : _ t) : _ option = Queue.take_opt self.actions

let pop_actions (self : _ t) : 'action iter =
  if Queue.is_empty self.actions then
    ignore
  else (
    let q = Queue.create () in
    Queue.transfer self.actions q;
    fun yield -> Queue.iter yield q
  )

let transfer_actions (st1 : _ t) (st2 : _ t) =
  Queue.transfer st1.actions st2.actions

(** Add external input to the state machine *)
let add_input self i : unit = Queue.push i self.inputs

(** Perform some work *)
let work ?transfer_to_aq self : _ res =
  let rec loop (next : _ next) =
    match next with
    | N_return x ->
      self.next <- next;
      Ok x
    | N_fail err ->
      self.next <- next;
      Error err
    | N_run f -> f () |> loop
    | N_await f -> run_on_next_input f
  and run_on_next_input f =
    match Queue.take self.inputs with
    | exception Queue.Empty ->
      self.next <- N_await f;
      Working
    | input -> loop (f input)
  in
  let r = loop self.next in
  Option.iter (fun aq -> Queue.transfer self.actions aq.aq) transfer_to_aq;
  r
