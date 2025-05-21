(** Nano event loop *)

include module type of struct
  include Nanoev
end

val create : unit -> t
(** Create a [Nanoev.t] based on [Unix.select]. This is fairly limited and only
    works in processes that have fewer than 1024 FDs open in total. *)
