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
