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

let%expect_test "p03" =
  print_s [%sexp (p03 [ 1; 2; 3 ] 2 : int)];
  [%expect {| 2 |}];
  print_s [%sexp (p03 ("ocaml" |> String.to_list) 4 : char)];
  [%expect {| m |}]
;;

let%expect_test "p04" =
  print_s [%sexp (p04 [ 123; 456; 789 ] : int)];
  [%expect {| 3 |}];
  print_s [%sexp (p04 ("Hello, world!" |> String.to_list) : int)];
  [%expect {| 13 |}]
;;

let%expect_test "p05" =
  print_s
    [%sexp
      ( p05 ("A man, a plan, a canal, panama!" |> String.to_list) |> String.of_char_list
        : string )];
  [%expect {| "!amanap ,lanac a ,nalp a ,nam A" |}];
  print_s [%sexp (p05 [ 1; 2; 3; 4 ] : int list)];
  [%expect {| (4 3 2 1) |}]
;;
