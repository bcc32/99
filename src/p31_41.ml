open! Base

let is_prime n =
  match n with
  | n
    when n < 2 -> false
  | 2 -> true
  | n
    when n % 2 = 0 -> false
  | n ->
    let rec loop i =
      if i * i > n then true else if n % i = 0 then false else loop (i + 2)
    in
    loop 3
;;

let p31 = is_prime

let gcd a b =
  let rec loop a b =
    match a, b with
    | 0, x
    | x, 0 -> x
    | a, b -> loop b (a % b)
  in
  loop (Int.abs a) (Int.abs b)
;;

let p32 = gcd
let is_coprime a b = gcd a b = 1
let p33 = is_coprime

let totient n =
  if n = 1
  then 1
  else (
    let rec loop i acc =
      if i < n then loop (i + 1) (if is_coprime i n then acc + 1 else acc) else acc
    in
    loop 1 0)
;;

let p34 = totient

let prime_factors n =
  let rec loop i n acc =
    if i * i > n
    then List.rev (if n > 1 then n :: acc else acc)
    else if n % i = 0
    then loop i (n / i) (i :: acc)
    else loop (if i = 2 then 3 else i + 2) n acc
  in
  loop 2 n []
;;

let p35 = prime_factors

let prime_factorize n =
  prime_factors n
  |> P01_10.Private.run_length_encode ~equal:Int.equal
  |> List.map ~f:(fun (mult, prime) -> prime, mult)
;;

let p36 = prime_factorize

let int_pow a b =
  let rec loop a b acc =
    if b = 0
    then acc
    else if b % 2 = 0
    then loop (a * a) (b / 2) acc
    else loop a (b - 1) (a * acc)
  in
  loop a b 1
;;

let totient n =
  prime_factorize n
  |> List.map ~f:(fun (prime, mult) -> (prime - 1) * int_pow prime (mult - 1))
  |> List.fold ~init:1 ~f:( * )
;;

let p37 = totient
let prime_range a b = List.range a b ~stop:`inclusive |> List.filter ~f:is_prime
let p39 = prime_range
