open! Base

type 'a t =
  | Empty
  | Node of 'a * 'a t * 'a t
[@@deriving sexp]

let rec is_isomorphic t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> is_isomorphic l1 l2 && is_isomorphic r1 r2
  | _ -> false
;;

let rec mirror = function
  | Empty -> Empty
  | Node (x, l, r) -> Node (x, mirror r, mirror l)
;;
