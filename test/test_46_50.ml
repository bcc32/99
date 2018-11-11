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

let%expect_test "p49" =
  print_s [%sexp (p49 3 : string list)];
  [%expect {| (000 001 011 010 110 111 101 100) |}]
;;

let%expect_test "p50" =
  let frequencies = [ 'a', 45; 'b', 13; 'c', 12; 'd', 16; 'e', 9; 'f', 5 ] in
  print_s [%sexp (p50 frequencies ~compare:Char.compare : (char * string) list)];
  [%expect {|
    ((a 0)
     (b 101)
     (c 100)
     (d 111)
     (e 1101)
     (f 1100)) |}]
;;
