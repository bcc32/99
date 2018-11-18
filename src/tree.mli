open! Base

type 'a t =
  | Empty
  | Node of 'a * 'a t * 'a t
[@@deriving sexp]

val is_isomorphic : _ t -> _ t -> bool
val mirror : 'a t -> 'a t
val add : 'a t -> compare:('a -> 'a -> int) -> 'a -> 'a t
val count_nodes : _ t -> int
