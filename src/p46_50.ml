open! Base

let truth_table a b expr =
  let row a_val b_val =
    ( [ a_val; b_val ]
    , Blang.eval expr ~env:(Map.of_alist_exn (module String) [ a, a_val; b, b_val ]) )
  in
  [ row false false; row false true; row true false; row true true ]
;;

let p46 ~a ~b expr = truth_table a b expr
