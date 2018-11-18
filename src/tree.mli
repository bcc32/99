open! Base

type 'a t =
  | Empty
  | Node of 'a * 'a t * 'a t
[@@deriving sexp]

val is_isomorphic : _ t -> _ t -> bool
val mirror : 'a t -> 'a t
val add : 'a t -> compare:('a -> 'a -> int) -> 'a -> 'a t

(** in-order traversal of nodes *)
val fold_nodes : 'a t -> init:'b -> f:('b -> 'a * 'a t * 'a t -> 'b) -> 'b
