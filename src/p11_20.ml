open! Base

let one_or_many_of_pair =
  let open One_or_many in
  function
  | 1, x -> One x
  | n, x -> Many (n, x)
;;

let run_length_encode' list ~equal =
  P01_10.Private.run_length_encode list ~equal |> List.map ~f:one_or_many_of_pair
;;

let p11 = run_length_encode'

let make_list n x =
  let rec loop n acc =
    match n with
    | 0 -> acc
    | n -> loop (n - 1) (x :: acc)
  in
  loop n []
;;

let list_of_one_or_many (x : _ One_or_many.t) =
  match x with
  | One x -> [ x ]
  | Many (n, x) -> make_list n x
;;

let run_length_decode' elts = P01_10.Private.concat_map elts ~f:list_of_one_or_many
let p12 = run_length_decode'

let run_length_encode_direct list ~equal =
  match list with
  | [] -> []
  | prev :: list ->
    let rec loop list prev count acc =
      match list with
      | [] -> List.rev (one_or_many_of_pair (count, prev) :: acc)
      | hd :: tl
        when equal prev hd -> loop tl hd (count + 1) acc
      | hd :: tl -> loop tl hd 1 (one_or_many_of_pair (count, prev) :: acc)
    in
    loop list prev 1 []
;;

let p13 = run_length_encode_direct
let duplicate list = P01_10.Private.concat_map list ~f:(fun x -> [ x; x ])
let p14 = duplicate
let replicate list n = P01_10.Private.concat_map list ~f:(fun x -> make_list n x)
let p15 = replicate

let drop_every list n =
  let rec loop list i accum =
    match list with
    | [] -> List.rev accum
    | hd :: tl -> if i = 0 then loop tl (n - 1) accum else loop tl (i - 1) (hd :: accum)
  in
  loop list (n - 1) []
;;

let p16 = drop_every

let split_at list n =
  if n < 0 then raise_s [%message "out of bounds" (n : int)];
  let rec loop list i accum =
    if i = 0
    then List.rev accum, list
    else (
      match list with
      | [] -> raise_s [%message "out of bounds" (n : int)]
      | hd :: tl -> loop tl (i - 1) (hd :: accum))
  in
  loop list n []
;;

let p17 = split_at

let slice_incl list a b =
  let _, list = split_at list (a - 1) in
  let list, _ = split_at list (b - a + 1) in
  list
;;

let p18 = slice_incl

let rotate_left list n =
  let n = if n < 0 then List.length list + n else n in
  let l, r = split_at list n in
  r @ l
;;

let p19 = rotate_left

let pop_at list n =
  let prefix, list = split_at list (n - 1) in
  match list with
  | [] -> raise_s [%message "out of bounds" (n : int)]
  | elt :: suffix -> elt, prefix @ suffix
;;

let p20 n list = pop_at list n
