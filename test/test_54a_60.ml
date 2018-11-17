open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p55" =
  print_s [%sexp (p55 4 : char Tree.t list)];
  [%expect
    {|
    ((Node x (Node x Empty Empty) (Node x Empty (Node x Empty Empty)))
     (Node x (Node x Empty Empty) (Node x (Node x Empty Empty) Empty))
     (Node x (Node x Empty (Node x Empty Empty)) (Node x Empty Empty))
     (Node x (Node x (Node x Empty Empty) Empty) (Node x Empty Empty))) |}]
;;

let%expect_test "p56" =
  print_s [%sexp (p56 (Node ('x', Node ('x', Empty, Empty), Empty)) : bool)];
  [%expect {| false |}];
  print_s
    [%sexp (p56 (Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))) : bool)];
  [%expect {| true |}]
;;
