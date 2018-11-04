open! Base

val p11 : 'a list -> equal:'a Equal.t -> 'a One_or_many.t list
val p12 : 'a One_or_many.t list -> 'a list
val p13 : 'a list -> equal:'a Equal.t -> 'a One_or_many.t list
val p14 : 'a list -> 'a list
val p15 : 'a list -> int -> 'a list
val p16 : 'a list -> int -> 'a list
val p17 : 'a list -> int -> 'a list * 'a list
