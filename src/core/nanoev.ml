module Trace_ = Trace_

module FD = struct
  type t = {
    fd: Unix.file_descr;
    closed: bool Atomic.t;
  }

  let close self =
    if not (Atomic.exchange self.closed true) then Unix.close self.fd
end

module Impl = struct
  type 'st ops = {
    clear: 'st -> unit;
    get_fd: 'st -> Unix.file_descr -> FD.t;
    wakeup_from_outside: 'st -> unit;
    on_readable: 'a 'b. 'st -> FD.t -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    on_writable: 'a 'b. 'st -> FD.t -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    run_after_s: 'a 'b. 'st -> float -> 'a -> 'b -> ('a -> 'b -> unit) -> unit;
    step: 'st -> unit;
  }

  type t = Ev : 'a ops * 'a -> t

  let[@inline] build ops st : t = Ev (ops, st)
end

open Impl

type t = Impl.t

let[@inline] clear (Ev (ops, st)) = ops.clear st
let[@inline] get_fd (Ev (ops, st)) fd = ops.get_fd st fd
let[@inline] wakeup_from_outside (Ev (ops, st)) = ops.wakeup_from_outside st

let[@inline] on_readable (Ev (ops, st)) fd x y f : unit =
  ops.on_readable st fd x y f

let[@inline] on_writable (Ev (ops, st)) fd x y f : unit =
  ops.on_writable st fd x y f

let[@inline] run_after_s (Ev (ops, st)) time x y f : unit =
  ops.run_after_s st time x y f

let[@inline] step (Ev (ops, st)) : unit = ops.step st
