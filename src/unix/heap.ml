type 'a tree =
  | E
  | N of int * 'a * 'a tree * 'a tree

type 'a t = {
  leq: 'a -> 'a -> bool;
  mutable t: 'a tree;
}

let create ~leq () : _ t = { leq; t = E }

let[@inline] is_empty (self : _ t) =
  match self.t with
  | E -> true
  | N _ -> false

exception Empty

open struct
  (** Rank of the tree *)
  let[@inline] rank_ = function
    | E -> 0
    | N (r, _, _, _) -> r

  (** Make a balanced node labelled with [x], and subtrees [a] and [b].
     We ensure that the right child's rank is ≤ to the rank of the
     left child (leftist property). The rank of the resulting node
     is the length of the rightmost path. *)
  let[@inline] mk_node_ x a b =
    if rank_ a >= rank_ b then
      N (rank_ b + 1, x, a, b)
    else
      N (rank_ a + 1, x, b, a)

  let rec merge ~leq t1 t2 =
    match t1, t2 with
    | t, E -> t
    | E, t -> t
    | N (_, x, a1, b1), N (_, y, a2, b2) ->
      if leq x y then
        mk_node_ x a1 (merge ~leq b1 t2)
      else
        mk_node_ y a2 (merge ~leq t1 b2)
end

let clear self = self.t <- E

let[@inline] insert (self : _ t) x : unit =
  self.t <- merge ~leq:self.leq self.t (N (1, x, E, E))

let[@inline] peek_min_exn (self : _ t) =
  match self.t with
  | E -> raise Empty
  | N (_, x, _, _) -> x

let[@inline] pop_min_exn (self : _ t) =
  match self.t with
  | E -> raise Empty
  | N (_, x, l, r) ->
    self.t <- merge ~leq:self.leq l r;
    x
