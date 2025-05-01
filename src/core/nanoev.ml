module Trace_ = Trace_

exception Closed

module Impl = struct
  type 'st ops = {
    clear: 'st -> unit;
    wakeup_from_outside: 'st -> unit;
    close: 'st -> Unix.file_descr -> unit;
    max_fds: 'st -> int;
    on_readable:
      'a 'b.
      'st ->
      Unix.file_descr ->
      'a ->
      'b ->
      (closed:bool -> 'a -> 'b -> unit) ->
      unit;
    on_writable:
      'a 'b.
      'st ->
      Unix.file_descr ->
      'a ->
      'b ->
      (closed:bool -> 'a -> 'b -> unit) ->
      unit;
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
let[@inline] close (Ev (ops, st)) fd = ops.close st fd
let[@inline] max_fds (Ev (ops, st)) = ops.max_fds st

let[@inline] on_readable (Ev (ops, st)) fd x y f : unit =
  ops.on_readable st fd x y f

let[@inline] on_writable (Ev (ops, st)) fd x y f : unit =
  ops.on_writable st fd x y f

let[@inline] run_after_s (Ev (ops, st)) time x y f : unit =
  ops.run_after_s st time x y f

let[@inline] step (Ev (ops, st)) : unit = ops.step st
