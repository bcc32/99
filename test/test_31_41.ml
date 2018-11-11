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
