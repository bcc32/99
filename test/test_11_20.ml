open! Core_kernel
open! Import
open Ninety_nine

let%expect_test "p11" =
  print_s
    [%sexp
      ( p11 ("aaaabccaadeeee" |> String.to_list) ~equal:Char.equal
        : char One_or_many.t list )];
  [%expect {|
    ((Many 4 a)
     (One b)
     (Many 2 c)
     (Many 2 a)
     (One d)
     (Many 4 e)) |}]
;;
