open! Base

let insert_at elt list n =
  if n <= 0 then raise_s [%message "out of bounds" (n : int)];
  let rec loop list i acc =
    if i = 1
    then List.rev_append acc (elt :: list)
    else (
      match list with
      | [] -> raise_s [%message "out of bounds" (n : int)]
      | hd :: tl -> loop tl (i - 1) (hd :: acc))
  in
  loop list n []
;;

let p21 = insert_at

let iota a b =
  let rec loop i acc = if i < a then acc else loop (i - 1) (i :: acc) in
  loop b []
;;

let p22 = iota

let shuffle array ~random ~len =
  for i = 0 to len - 1 do
    let j = Random.State.int_incl random i (Array.length array - 1) in
    Array.swap array i j
  done
;;

let random_select ?(random = Random.State.default) elts n =
  let elts = Array.of_list elts in
  shuffle elts ~random ~len:n;
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (elts.(i) :: acc) in
  loop (n - 1) []
;;

let p23 = random_select
