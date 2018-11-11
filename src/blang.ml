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

let rec eval t ~env =
  let ( = ) = Bool.( = ) in
  let ( <> ) = Bool.( <> ) in
  match t with
  | Var v -> Map.find_exn env v
  | And (x, y) -> eval x ~env && eval y ~env
  | Or (x, y) -> eval x ~env || eval y ~env
  | Nand (x, y) -> not (eval x ~env && eval y ~env)
  | Nor (x, y) -> not (eval x ~env || eval y ~env)
  | Xor (x, y) -> eval x ~env <> eval y ~env
  | Impl (x, y) -> not (eval x ~env) || eval y ~env
  | Equ (x, y) -> eval x ~env = eval y ~env
  | Not x -> not (eval x ~env)
;;

let rec vars t =
  match t with
  | Var v -> Set.singleton (module String) v
  | And (x, y)
  | Or (x, y)
  | Nand (x, y)
  | Nor (x, y)
  | Xor (x, y)
  | Impl (x, y)
  | Equ (x, y) -> Set.union (vars x) (vars y)
  | Not x -> vars x
;;

module Truth_table = struct
  module Row = struct
    type t = bool list * bool [@@deriving sexp]
  end

  type t = Row.t list [@@deriving sexp]
end

module O = struct
  let var v = Var v
  let ( & ) a b = And (a, b)
  let ( or ) a b = Or (a, b)
  let ( !& ) a b = Nand (a, b)
  let ( !| ) a b = Nor (a, b)
  let ( ^ ) a b = Xor (a, b)
  let ( ==> ) a b = Impl (a, b)
  let ( <=> ) a b = Equ (a, b)
  let not t = Not t
  let ( := ) a b = Equ (a, b)
end

include O
