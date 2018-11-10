open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p21" =
  print_s [%sexp (p21 'X' ("abcd" |> String.to_list) 2 |> String.of_char_list : string)];
  [%expect {| aXbcd |}]
;;

let%expect_test "p22" =
  print_s [%sexp (p22 4 9 : int list)];
  [%expect {| (4 5 6 7 8 9) |}]
;;

let%expect_test "p23" =
  let random = Random.State.make [| 23 |] in
  print_s [%sexp (p23 ("abcdefgh" |> String.to_list) 3 ~random : char list)];
  [%expect {| (d h g) |}]
;;

let%expect_test "p24" =
  let random = Random.State.make [| 24 |] in
  print_s [%sexp (p24 6 49 ~random : int list)];
  [%expect {| (4 27 10 12 13 34) |}]
;;

let%expect_test "p25" =
  let random = Random.State.make [| 25 |] in
  print_s
    [%sexp (p25 ("abcdef" |> String.to_list) ~random |> String.of_char_list : string)];
  [%expect {| dafbce |}]
;;

let%expect_test "p26" =
  let combinations =
    p26 3 ("abcdef" |> String.to_list) |> List.map ~f:String.of_char_list
  in
  print_s [%sexp (List.length combinations : int), (combinations : string list)];
  [%expect
    {|
    (20 (
      abc
      abd
      abe
      abf
      acd
      ace
      acf
      ade
      adf
      aef
      bcd
      bce
      bcf
      bde
      bdf
      bef
      cde
      cdf
      cef
      def)) |}]
;;
