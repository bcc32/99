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

let%expect_test "p61a" =
  print_s [%sexp (p61a tree4 : int list)];
  [%expect {| (4 2) |}]
;;

let%expect_test "p62" =
  print_s [%sexp (p62 tree4 : int list)];
  [%expect {| (1 2) |}]
;;

let%expect_test "p62b" =
  print_s [%sexp (p62b tree4 2 : int list)];
  [%expect {| (2 2) |}]
;;
