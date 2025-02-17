include Cohttp

open struct
  module Slice = Iostream.Slice
end

module Base = struct
  type 'a io = 'a
  type 'a with_context = 'a

  type body =
    [ `String of string
    | `Stream of Iostream.In_buf.t
    ]

  let map_context : 'a with_context -> ('a -> 'b) -> 'b with_context = ( |> )

  (*
  type body

  val map_context : 'a with_context -> ('a -> 'b) -> 'b with_context

  val call :
    (?headers:Http.Header.t ->
    ?body:body ->
    ?chunked:bool ->
    Http.Method.t ->
    Uri.t ->
    (Http.Response.t * body) io)
    with_context
  (** [call ?headers ?body ?chunked meth uri]

      @return
        [(response, response_body)] Consume [response_body] in a timely fashion.
        Please see {!val:call} about how and why.
      @param chunked
        use chunked encoding if [true]. The default is [false] for compatibility
        reasons. *)
  *)
end

module IO = struct
  type 'a t = 'a

  let ( >>= ) = ( |> )
  let return = Fun.id

  type ic = Iostream.In_buf.t
  type oc = Iostream.Out_buf.t
  type conn = Unix.sockaddr

  let refill (ic : ic) : [ `Eof | `Ok ] =
    let slice = Iostream.In_buf.fill_buf ic in
    if slice.len = 0 then
      `Eof
    else
      `Ok

  let with_input_buffer (ic : ic) ~f =
    let slice = Iostream.In_buf.fill_buf ic in
    let res, consumed =
      f (Bytes.unsafe_to_string slice.bytes) ~pos:0 ~len:slice.len
    in
    Iostream.In_buf.consume ic consumed;
    res

  let read_line = Iostream.In_buf.input_line

  let read (ic : ic) (n : int) : string =
    let bs = Bytes.create n in
    let off = ref 0 in
    let missing = ref n in
    while !missing > 0 do
      let n = Iostream.In_buf.input ic bs !off !missing in
      off := !off + n;
      missing := !missing - n
    done;
    Bytes.unsafe_to_string bs

  let write = Iostream.Out_buf.output_string
  let flush = Iostream.Out_buf.flush
end

module Client = Cohttp.Generic.Client.Make (Base) (IO)
module Server = Cohttp.Generic.Server.Make (Base) (IO)
