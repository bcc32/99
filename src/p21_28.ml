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
