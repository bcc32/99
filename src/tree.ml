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

let rec add t ~compare elt =
  match t with
  | Empty -> Node (elt, Empty, Empty)
  | Node (elt', l, r) ->
    if compare elt elt' <= 0
    then Node (elt', add l ~compare elt, r)
    else Node (elt', l, add r ~compare elt)
;;

let rec count_nodes = function
  | Empty -> 0
  | Node (_, l, r) -> 1 + count_nodes l + count_nodes r
;;

let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r
;;
