(** Nano event loop *)

include module type of struct
  include Nanoev
end

val create : unit -> t
