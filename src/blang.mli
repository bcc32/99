open! Base

type t =
  | Var of string
  | And of t * t
  | Or of t * t
  | Nand of t * t
  | Nor of t * t
  | Xor of t * t
  | Impl of t * t
  | Equ of t * t
[@@deriving sexp]

val eval : t -> env:bool Map.M(String).t -> bool
val vars : t -> Set.M(String).t

module Truth_table : sig
  module Row : sig
    (** variables, then result *)
    type t = bool list * bool [@@deriving sexp]
  end

  type t = Row.t list [@@deriving sexp]
end
