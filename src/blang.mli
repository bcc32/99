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
  | Not of t
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

module O : sig
  val var : string -> t
  val ( & ) : t -> t -> t
  val ( or ) : t -> t -> t
  val ( !& ) : t -> t -> t
  val ( !| ) : t -> t -> t
  val ( ^ ) : t -> t -> t
  val ( ==> ) : t -> t -> t
  val ( <=> ) : t -> t -> t
  val not : t -> t

  (** low-precedence version of (<=>) *)
  val ( := ) : t -> t -> t
end

include module type of O
