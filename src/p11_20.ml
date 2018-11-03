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