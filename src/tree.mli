open! Base

type 'a t =
  | Empty
  | Node of 'a * 'a t * 'a t
[@@deriving sexp]

val is_isomorphic : _ t -> _ t -> bool
val mirror : 'a t -> 'a t
val add : 'a t -> compare:('a -> 'a -> int) -> 'a -> 'a t

(** pre-order traversal of nodes *)
module Pre_order : sig
  type 'a node = 'a * 'a t * 'a t
  type 'a t

  val fold_level : 'a t -> init:'b -> f:('b -> int -> 'a node -> 'b) -> 'b

  include Container.S1 with type 'a t := 'a node t
end
