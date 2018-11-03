open! Base

type 'a t =
  | One of 'a
  | Many of int * 'a
[@@deriving sexp_of]
