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

let%expect_test "p16" =
  print_s
    [%sexp (p16 ("abcdefghik" |> String.to_list) 3 |> String.of_char_list : string)];
  [%expect {| abdeghk |}]
;;

let%expect_test "p17" =
  let fst, snd = p17 ("abcdefghik" |> String.to_list) 3 in
  print_s
    [%sexp (fst |> String.of_char_list : string), (snd |> String.of_char_list : string)];
  [%expect {| (abc defghik) |}]
;;

let%expect_test "p18" =
  print_s
    [%sexp (p18 ("abcdefghik" |> String.to_list) 3 7 |> String.of_char_list : string)];
  [%expect {| cdefg |}]
;;

let%expect_test "p19" =
  print_s [%sexp (p19 ("abcdefgh" |> String.to_list) 3 |> String.of_char_list : string)];
  [%expect {| defghabc |}];
  print_s
    [%sexp (p19 ("abcdefgh" |> String.to_list) (-2) |> String.of_char_list : string)];
  [%expect {| ghabcdef |}]
;;
