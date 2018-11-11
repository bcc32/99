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
