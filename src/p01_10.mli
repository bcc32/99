open! Base

val p01 : 'a list -> 'a
val p02 : 'a list -> 'a
val p03 : 'a list -> int -> 'a
val p04 : _ list -> int
val p05 : 'a list -> 'a list
val p06 : 'a list -> equal:'a Equal.t -> bool
val p07 : 'a Nested_list.t -> 'a list
val p08 : 'a list -> equal:'a Equal.t -> 'a list
val p09 : 'a list -> equal:'a Equal.t -> 'a list list
val p10 : 'a list -> equal:'a Equal.t -> (int * 'a) list
