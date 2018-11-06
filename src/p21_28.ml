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

let list_of_array array ~len =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (array.(i) :: acc) in
  loop (len - 1) []
;;

let random_select ?(random = Random.State.default) elts n =
  let elts = Array.of_list elts in
  shuffle elts ~random ~len:n;
  list_of_array elts ~len:n
;;

let p23 = random_select

let lotto_select ?(random = Random.State.default) len max =
  let elts = iota 1 max |> Array.of_list in
  shuffle elts ~random ~len;
  list_of_array elts ~len
;;

let p24 = lotto_select

let shuffle_list ?(random = Random.State.default) list =
  let elts = Array.of_list list in
  shuffle elts ~random ~len:(Array.length elts);
  list_of_array elts ~len:(Array.length elts)
;;

let p25 = shuffle_list
