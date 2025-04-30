(** Nano event loop using Poll/Ppoll *)

include module type of struct
  include Nanoev
end

val create : unit -> t
val create_with : Iomux.Poll.t -> t
