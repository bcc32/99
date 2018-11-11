open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p46" =
  let blang = Blang.(And (Var "a", Or (Var "a", Var "b"))) in
  print_s [%sexp (p46 ~a:"a" ~b:"b" blang : Blang.Truth_table.t)];
  [%expect {|
    (((false false) false)
     ((false true)  false)
     ((true  false) true)
     ((true  true)  true)) |}]
;;
