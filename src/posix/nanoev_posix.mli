(** Nano event loop using Poll/Ppoll *)

include module type of struct
  include Nanoev
end

val create : unit -> t
(** Create a new nanoev loop using [Iomux] (poll/ppoll).

    {b NOTE}: this is NOT thread-safe *)
