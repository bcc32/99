open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p31" =
  print_s [%sexp (p31 7 : bool)];
  [%expect {| true |}]
;;
