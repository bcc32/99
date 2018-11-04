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
