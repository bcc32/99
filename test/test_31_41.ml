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

let%expect_test "p33" =
  print_s [%sexp (p33 35 64 : bool)];
  [%expect {| true |}]
;;

let%expect_test "p34" =
  print_s [%sexp (p34 10 : int)];
  [%expect {| 4 |}]
;;

let%expect_test "p35" =
  print_s [%sexp (p35 315 : int list)];
  [%expect {| (3 3 5 7) |}]
;;

let%expect_test "p36" =
  print_s [%sexp (p36 315 : (int * int) list)];
  [%expect {|
    ((3 2)
     (5 1)
     (7 1)) |}]
;;

let%expect_test "p37" =
  print_s [%sexp (p37 10 : int)];
  [%expect {| 4 |}]
;;

let%expect_test "p39" =
  print_s [%sexp (p39 10 20 : int list)];
  [%expect {| (11 13 17 19) |}]
;;

let%expect_test "p40" =
  print_s [%sexp (p40 28 : int * int)];
  [%expect {| (5 23) |}]
;;

let%expect_test "p41" =
  print_s [%sexp (p41 9 20 : (int * int) list)];
  [%expect {|
    ((3 7)
     (5 7)
     (3 11)
     (3 13)
     (5 13)
     (3 17)) |}];
  print_s [%sexp (p41 4 2000 ~lbound:50 : (int * int) list)];
  [%expect {|
    ((73 919)
     (61 1321)
     (67 1789)
     (61 1867)) |}]
;;
