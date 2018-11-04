open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p21" =
  print_s [%sexp (p21 'X' ("abcd" |> String.to_list) 2 |> String.of_char_list : string)];
  [%expect {| aXbcd |}]
;;
