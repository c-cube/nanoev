type 'a t

val create : leq:('a -> 'a -> bool) -> unit -> 'a t

val is_empty : _ t -> bool
(** [is_empty h] returns [true] if the heap [h] is empty. *)

exception Empty

val clear : _ t -> unit
val insert : 'a t -> 'a -> unit
val peek_min_exn : 'a t -> 'a
val pop_min_exn : 'a t -> 'a
