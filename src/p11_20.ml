open! Base

let one_or_many_of_pair =
  let open One_or_many in
  function
  | 1, x -> One x
  | n, x -> Many (n, x)
;;

let run_length_encode' list ~equal =
  let open P01_10.Private in
  run_length_encode list ~equal |> List.map ~f:one_or_many_of_pair
;;

let p11 = run_length_encode'
