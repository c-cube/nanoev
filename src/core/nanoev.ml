module Trace_ = Trace_

module Impl = struct
  type 'st ops = {
    clear: 'st -> unit;
    wakeup_from_outside: 'st -> unit;
    on_readable:
      'a 'b. 'st -> Unix.file_descr -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    on_writable:
      'a 'b. 'st -> Unix.file_descr -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    run_after_s: 'a 'b. 'st -> float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    step: 'st -> unit;
  }

  type t = Ev : 'a ops * 'a -> t

  let[@inline] build ops st : t = Ev (ops, st)
end

open Impl

type t = Impl.t

let[@inline] clear (Ev (ops, st)) = ops.clear st
let[@inline] wakeup_from_outside (Ev (ops, st)) = ops.wakeup_from_outside st

let[@inline] on_readable (Ev (ops, st)) fd x y f : unit =
  ops.on_readable st fd x y f

let[@inline] on_writable (Ev (ops, st)) fd x y f : unit =
  ops.on_writable st fd x y f

let[@inline] run_after_s (Ev (ops, st)) time x y f : unit =
  ops.run_after_s st time x y f

let[@inline] step (Ev (ops, st)) : unit = ops.step st

(*
let rec read (self:t) fd buf i len : int =
  match Unix.read fd buf i len with
    | n -> n
  | exception Unix.Unix_error (Unix, _, _) ->
    read self fd buf i len
*)
