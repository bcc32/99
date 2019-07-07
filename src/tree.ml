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

type 'a order = 'a * 'a t * 'a t

module Pre_order = struct
  let fold_level t ~init ~f =
    let rec loop t depth ~init ~f =
      match t with
      | Empty -> init
      | Node (elt, l, r) ->
        let init = f init depth (elt, l, r) in
        let init = loop l (depth + 1) ~init ~f in
        let init = loop r (depth + 1) ~init ~f in
        init
    in
    loop t 0 ~init ~f
  ;;

  module For_container = struct
    type nonrec 'a t = 'a t

    let fold (elt, l, r) ~init ~f =
      fold_level (Node (elt, l, r)) ~init ~f:(fun acc _ node -> f acc node)
    ;;

    let iter = `Define_using_fold
  end

  include Container.Make (For_container)
end
