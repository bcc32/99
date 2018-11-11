open! Base

let truth_table vars expr =
  let row vals =
    ( vals
    , Blang.eval expr ~env:(Map.of_alist_exn (module String) (List.zip_exn vars vals)) )
  in
  let rec list_bools n =
    if n = 0
    then [ [] ]
    else (
      let x = list_bools (n - 1) in
      List.map x ~f:(fun bs -> false :: bs) @ List.map x ~f:(fun bs -> true :: bs))
  in
  list_bools (List.length vars) |> List.map ~f:row
;;

let p46 ~a ~b expr = truth_table [ a; b ] expr
let p47 ~a ~b expr = truth_table [ a; b ] expr
let p48 = truth_table

let rec gray_code =
  let cache = Queue.create () in
  fun n ->
    if n = 0
    then [ "" ]
    else if n < Queue.length cache
    then Queue.get cache n
    else (
      let x = gray_code (n - 1) in
      List.rev_append
        (List.rev_map x ~f:(fun x -> "0" ^ x))
        (List.rev_map x ~f:(fun x -> "1" ^ x)))
;;

let p49 = gray_code

module Heap : sig
  type 'a t

  val empty : _ t
  val is_singleton : 'a t -> bool
  val add : 'a t -> 'a -> compare:('a -> 'a -> int) -> 'a t
  val top_exn : 'a t -> 'a
  val pop_exn : 'a t -> compare:('a -> 'a -> int) -> 'a * 'a t
end = struct
  type 'a t =
    | Empty
    | Node of { elt : 'a; subtrees : 'a t list }

  let empty = Empty

  let is_singleton = function
    | Node { elt = _; subtrees = [] } -> true
    | _ -> false
  ;;

  let top_exn t =
    match t with
    | Empty -> raise Caml.Not_found
    | Node { elt; subtrees = _ } -> elt
  ;;

  let merge a b ~compare =
    match a, b with
    | Empty, x
    | x, Empty -> x
    | ( Node { elt = a_elt; subtrees = a_subtrees }
      , Node { elt = b_elt; subtrees = b_subtrees } ) ->
      if compare a_elt b_elt <= 0
      then Node { elt = a_elt; subtrees = b :: a_subtrees }
      else Node { elt = b_elt; subtrees = a :: b_subtrees }
  ;;

  let add t x ~compare = merge t (Node { elt = x; subtrees = [] }) ~compare

  let rec merge_pairs ts ~compare =
    match ts with
    | [] -> Empty
    | [ t ] -> t
    | x :: y :: tl ->
      let merge = merge ~compare in
      merge (merge x y) (merge_pairs tl ~compare)
  ;;

  let remove_top_exn t =
    match t with
    | Empty -> raise Caml.Not_found
    | Node { elt = _; subtrees } -> merge_pairs subtrees
  ;;

  let pop_exn t ~compare = top_exn t, remove_top_exn t ~compare
end

module Huffman_tree = struct
  type 'a t =
    | Leaf of 'a
    | Node of { zero : 'a t; one : 'a t }
  [@@deriving compare]

  let rec to_list = function
    | Leaf x -> [ x, "" ]
    | Node { zero; one } ->
      List.map (to_list zero) ~f:(fun (x, code) -> x, "0" ^ code)
      @ List.map (to_list one) ~f:(fun (x, code) -> x, "1" ^ code)
  ;;
end

let huffman_code (type a) frequencies ~compare:compare_a =
  let compare = [%compare: int * a Huffman_tree.t] in
  let heap =
    List.fold frequencies ~init:Heap.empty ~f:(fun acc (elt, freq) ->
      Heap.add acc (freq, Huffman_tree.Leaf elt) ~compare)
  in
  let merge (freq_a, a) (freq_b, b) =
    freq_a + freq_b, Huffman_tree.Node { zero = a; one = b }
  in
  let rec loop heap =
    if Heap.is_singleton heap
    then
      Heap.top_exn heap
      |> snd
      |> Huffman_tree.to_list
      |> List.sort ~compare:[%compare: a * string]
    else (
      let a, heap = Heap.pop_exn heap ~compare in
      let b, heap = Heap.pop_exn heap ~compare in
      let heap = Heap.add heap (merge a b) ~compare in
      loop heap)
  in
  loop heap
;;

let p50 = huffman_code
