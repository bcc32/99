open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p46" =
  let blang = Blang.(And (Var "a", Or (Var "a", Var "b"))) in
  print_s [%sexp (p46 ~a:"a" ~b:"b" blang : Blang.Truth_table.t)];
  [%expect
    {|
    (((false false) false)
     ((false true)  false)
     ((true  false) true)
     ((true  true)  true)) |}]
;;

let%expect_test "p47" =
  let blang = Blang.(var "a" & (var "a" or not (var "b"))) in
  print_s [%sexp (p47 ~a:"a" ~b:"b" blang : Blang.Truth_table.t)];
  [%expect
    {|
    (((false false) false)
     ((false true)  false)
     ((true  false) true)
     ((true  true)  true)) |}]
;;

let%expect_test "p48" =
  let blang =
    Blang.(var "a" & (var "b" or var "c") := (var "a" & var "b") or (var "a" & var "c"))
  in
  print_s [%sexp (p48 [ "a"; "b"; "c" ] blang : Blang.Truth_table.t)];
  [%expect
    {|
    (((false false false) true)
     ((false false true)  true)
     ((false true  false) true)
     ((false true  true)  true)
     ((true  false false) true)
     ((true  false true)  true)
     ((true  true  false) true)
     ((true  true  true)  true)) |}];
  let blang =
    Blang.((var "a" & (var "b" or var "c") <=> var "a" & var "b") or (var "a" & var "c"))
  in
  print_s [%sexp (p48 [ "a"; "b"; "c" ] blang : Blang.Truth_table.t)];
  [%expect
    {|
    (((false false false) false)
     ((false false true)  false)
     ((false true  false) false)
     ((false true  true)  false)
     ((true  false false) false)
     ((true  false true)  true)
     ((true  true  false) true)
     ((true  true  true)  true)) |}]
;;
