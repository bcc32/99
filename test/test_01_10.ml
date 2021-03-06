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

let%expect_test "p06" =
  print_s [%sexp (p06 [ 1; 2; 3 ] ~equal:Int.equal : bool)];
  [%expect {| false |}];
  print_s [%sexp (p06 ("madamimadam" |> String.to_list) ~equal:Char.equal : bool)];
  [%expect {| true |}];
  print_s [%sexp (p06 [ 1; 2; 4; 8; 16; 8; 4; 2; 1 ] ~equal:Int.equal : bool)];
  [%expect {| true |}]
;;

let%expect_test "p07" =
  let open Nested_list in
  print_s [%sexp (p07 (Atom 5) : int list)];
  [%expect {| (5) |}];
  print_s
    [%sexp
      (p07 (List [ Atom 1; List [ Atom 2; List [ Atom 3; Atom 4 ]; Atom 5 ] ]) : int list)];
  [%expect {| (1 2 3 4 5) |}];
  print_s [%sexp (p07 (List []) : int list)];
  [%expect {| () |}]
;;

let%expect_test "p08" =
  print_s
    [%sexp
      ( p08 ("aaaabccaadeeee" |> String.to_list) ~equal:Char.equal |> String.of_char_list
        : string )];
  [%expect {| abcade |}]
;;

let%expect_test "p09" =
  print_s
    [%sexp
      ( p09 ("aaaabccaadeeee" |> String.to_list) ~equal:Char.equal
        |> List.map ~f:String.of_char_list
        : string list )];
  [%expect {| (aaaa b cc aa d eeee) |}]
;;

let%expect_test "p10" =
  print_s
    [%sexp
      (p10 ("aaaabccaadeeee" |> String.to_list) ~equal:Char.equal : (int * char) list)];
  [%expect {|
    ((4 a)
     (1 b)
     (2 c)
     (2 a)
     (1 d)
     (4 e)) |}]
;;
