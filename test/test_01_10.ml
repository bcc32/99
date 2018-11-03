open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p01" =
  print_s [%sexp (p01 [ 1; 2; 3; 4 ] : int)];
  [%expect {| 4 |}];
  print_s [%sexp (p01 [ 'x'; 'y'; 'z' ] : char)];
  [%expect {| z |}]
;;

let%expect_test "p02" =
  print_s [%sexp (p02 [ 1; 2; 3; 4 ] : int)];
  [%expect {| 3 |}];
  let letters =
    List.range' 'a' 'z' ~compare:Char.compare ~stop:`inclusive ~stride:(fun x ->
      Char.unsafe_of_int (Char.to_int x + 1))
  in
  print_s [%sexp (p02 letters : char)];
  [%expect {| y |}]
;;
