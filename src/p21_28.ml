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

let combinations n list =
  let acc = ref [] in
  let rec loop n list elts_so_far =
    match n, list with
    | 0, _ -> acc := List.rev elts_so_far :: !acc
    | _, [] -> ()
    | n, hd :: tl ->
      loop (n - 1) tl (hd :: elts_so_far);
      loop n tl elts_so_far
  in
  loop n list [];
  List.rev !acc
;;

let p26 = combinations

let groupings sizes list =
  let acc = ref [] in
  let groups = Array.create [] ~len:(List.length sizes) in
  let sizes_remaining = Array.of_list sizes in
  let rec loop list =
    match list with
    | [] -> acc := (groups |> Array.to_list |> List.map ~f:List.rev) :: !acc
    | hd :: tl ->
      for i = 0 to Array.length sizes_remaining - 1 do
        if sizes_remaining.(i) > 0
        then (
          sizes_remaining.(i) <- sizes_remaining.(i) - 1;
          groups.(i) <- hd :: groups.(i);
          loop tl;
          groups.(i) <- List.tl_exn groups.(i);
          sizes_remaining.(i) <- sizes_remaining.(i) + 1)
      done
  in
  loop list;
  List.rev !acc
;;

let p27 = groupings
