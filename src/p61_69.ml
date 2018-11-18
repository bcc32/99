open! Base

let count_leaves =
  Tree.fold_nodes ~init:0 ~f:(fun acc -> function
    | _, Empty, Empty -> acc + 1
    | _ -> acc)
;;

let p61 = count_leaves

let leaves t =
  Tree.fold_nodes t ~init:[] ~f:(fun acc -> function
    | x, Empty, Empty -> x :: acc
    | _ -> acc)
  |> List.rev
;;

let p61a = leaves

let internals t =
  Tree.fold_nodes t ~init:[] ~f:(fun acc -> function
    | _, Empty, Empty -> acc
    | x, _, _ -> x :: acc)
  |> List.rev
;;

let p62 = internals
