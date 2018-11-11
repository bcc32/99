open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p31" =
  print_s [%sexp (p31 7 : bool)];
  [%expect {| true |}]
;;

let%expect_test "p32" =
  print_s [%sexp ([ p32 36 63; p32 (-3) (-6); p32 (-3) 6 ] : int list)];
  [%expect {| (9 3 3) |}]
;;

let%expect_test "p33" =
  print_s [%sexp (p33 35 64 : bool)];
  [%expect {| true |}]
;;

let%expect_test "p34" =
  print_s [%sexp (p34 10 : int)];
  [%expect {| 4 |}]
;;
