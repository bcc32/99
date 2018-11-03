open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p11" =
  print_s
    [%sexp
      ( p11 ("aaaabccaadeeee" |> String.to_list) ~equal:Char.equal
        : char One_or_many.t list )];
  [%expect
    {|
    ((Many 4 a)
     (One b)
     (Many 2 c)
     (Many 2 a)
     (One d)
     (Many 4 e)) |}]
;;

let%expect_test "p12" =
  let open One_or_many in
  print_s
    [%sexp
      ( p12
          [ Many (4, 'a'); One 'b'; Many (2, 'c'); Many (2, 'a'); One 'd'; Many (4, 'e') ]
        |> String.of_char_list
        : string )];
  [%expect {| aaaabccaadeeee |}]
;;

let%expect_test "p13" =
  print_s
    [%sexp
      ( p13 ("aaaabccaadeeee" |> String.to_list) ~equal:Char.equal
        : char One_or_many.t list )];
  [%expect
    {|
    ((Many 4 a)
     (One b)
     (Many 2 c)
     (Many 2 a)
     (One d)
     (Many 4 e)) |}]
;;

let%expect_test "p14" =
  print_s [%sexp (p14 [ 1; 2; 3 ] : int list)];
  [%expect {| (1 1 2 2 3 3) |}]
;;

let%expect_test "p15" =
  print_s [%sexp (p15 [ 'a'; 'b'; 'c' ] 3 : char list)];
  [%expect {| (a a a b b b c c c) |}]
;;
