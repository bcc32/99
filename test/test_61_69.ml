open! Core_kernel
open! Import
open Ninety_nine

let tree4 =
  Tree.(Node (1, Node (2, Empty, Node (4, Empty, Empty)), Node (2, Empty, Empty)))
;;

let%expect_test "p61" =
  print_s [%sexp (p61 tree4 : int)];
  [%expect {| 2 |}]
;;
