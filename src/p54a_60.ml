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

let construct_balanced_symmetric_trees n =
  construct_balanced_trees n |> List.filter ~f:is_symmetric
;;

let p58 = construct_balanced_symmetric_trees

let rec construct_height_balanced_trees =
  let cache = Hashtbl.create (module Int) in
  fun n ->
    Hashtbl.findi_or_add cache n ~default:(function
      | 0 -> [ Tree.Empty ]
      | 1 -> [ Tree.Node ('x', Empty, Empty) ]
      | n ->
        let n_2 = construct_height_balanced_trees (n - 2) in
        let n_1 = construct_height_balanced_trees (n - 1) in
        List.cartesian_product n_2 n_1
        @ List.cartesian_product n_1 n_2
        @ List.cartesian_product n_1 n_1
        |> List.map ~f:(fun (x, y) -> Tree.Node ('x', x, y)))
;;

let p59 = construct_height_balanced_trees
