open! Base

let p54a = ()

let rec construct_balanced_trees =
  let cache = Hashtbl.create (module Int) in
  fun n ->
    Hashtbl.findi_or_add cache n ~default:(fun n ->
      if n = 0
      then [ Tree.Empty ]
      else (
        let remainder = n - 1 in
        if remainder % 2 = 0
        then (
          let subtrees = construct_balanced_trees (remainder / 2) in
          List.cartesian_product subtrees subtrees
          |> List.map ~f:(fun (x, y) -> Tree.Node ('x', x, y)))
        else (
          let subtrees_smaller = construct_balanced_trees (remainder / 2) in
          let subtrees_bigger = construct_balanced_trees ((remainder / 2) + 1) in
          List.cartesian_product subtrees_smaller subtrees_bigger
          @ List.cartesian_product subtrees_bigger subtrees_smaller
          |> List.map ~f:(fun (x, y) -> Tree.Node ('x', x, y)))))
;;

let p55 = construct_balanced_trees
let is_symmetric t = Tree.is_isomorphic t (Tree.mirror t)
let p56 = is_symmetric

let construct_binary_tree elts ~compare =
  List.fold elts ~init:Tree.Empty ~f:(Tree.add ~compare)
;;

let p57 = construct_binary_tree
