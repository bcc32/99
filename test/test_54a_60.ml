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

let%expect_test "p57" =
  let show_is_symmetric elts =
    print_s [%sexp (p56 (p57 elts ~compare:Int.compare) : bool)]
  in
  print_s [%sexp (p57 [ 3; 2; 5; 7; 1 ] ~compare:Int.compare : int Tree.t)];
  [%expect
    {|
    (Node 3
      (Node 2 (Node 1 Empty Empty) Empty)
      (Node 5 Empty (Node 7 Empty Empty))) |}];
  show_is_symmetric [ 5; 3; 18; 1; 4; 12; 21 ];
  [%expect {| true |}];
  show_is_symmetric [ 3; 2; 5; 7; 1 ];
  [%expect {| true |}]
;;
