let is_setup = Global_ev.has_bg_thread
let setup = Global_ev.setup_bg_thread
let shutdown = Global_ev.shutdown_bg_thread

let with_setup ev f =
  setup ev;
  Fun.protect ~finally:shutdown f
