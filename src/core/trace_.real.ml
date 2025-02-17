let[@inline] with_span ?data ~__FILE__ ~__LINE__ name f =
  Trace_core.with_span ?data ~__FILE__ ~__LINE__ name f

let[@inline] message ?data m = Trace_core.message ?data m
let set_thread_name name = Trace_core.set_thread_name name
