let is_setup = Global_.has_bg_thread
let setup = Global_.setup_bg_thread
let shutdown = Global_.shutdown_bg_thread

let with_setup ev f =
  setup ev;
  Fun.protect ~finally:shutdown f
