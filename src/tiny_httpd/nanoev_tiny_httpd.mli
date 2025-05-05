module TH = Tiny_httpd_core

val create :
  ?enable_logging:bool ->
  ?masksigpipe:bool ->
  ?max_connections:int ->
  ?max_buf_pool_size:int ->
  ?timeout:float ->
  ?buf_size:int ->
  ?get_time_s:(unit -> float) ->
  ?addr:string ->
  ?port:int ->
  ?sock:Unix.file_descr ->
  ?head_middlewares:TH.Server.Head_middleware.t list ->
  ?middlewares:([ `Encoding | `Stage of int ] * TH.Server.Middleware.t) list ->
  new_thread:((unit -> unit) -> unit) ->
  unit ->
  TH.Server.t
