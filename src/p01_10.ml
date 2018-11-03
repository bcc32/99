open! Base

let rec last_exn = function
  | [] -> raise_s [%message "empty list"]
  | [ x ] -> x
  | _ :: tl -> last_exn tl
;;

let p01 = last_exn

let rec next_last_exn = function
  | ([] | [ _ ]) as l -> raise_s [%message "no second-to-last element" (l : _ list)]
  | [ x; _ ] -> x
  | _ :: tl -> next_last_exn tl
;;

let p02 = next_last_exn

let kth_exn list k =
  if k <= 0 then raise_s [%message "negative index" (k : int)];
  let rec loop list i =
    match list with
    | [] -> raise_s [%message "out of range" (k : int)]
    | hd :: _
      when i = 1 -> hd
    | _ :: tl -> loop tl (i - 1)
  in
  loop list k
;;

let p03 = kth_exn

let length list =
  let rec loop list acc =
    match list with
    | [] -> acc
    | _ :: tl -> loop tl (acc + 1)
  in
  loop list 0
;;

let p04 = length

let rec rev_append list ~onto =
  match list with
  | [] -> onto
  | hd :: tl -> rev_append tl ~onto:(hd :: onto)
;;

let rev list = rev_append list ~onto:[]
let p05 = rev

let rec is_equal x y ~equal =
  match x, y with
  | [], [] -> true
  | x :: xs, y :: ys
    when equal x y -> is_equal xs ys ~equal
  | _ -> false
;;

let is_palindrome list ~equal = is_equal list (rev list) ~equal
let p06 = is_palindrome

let concat_map list ~f =
  let rec loop list acc =
    match list with
    | [] -> rev acc
    | hd :: tl -> loop tl (rev_append (f hd) ~onto:acc)
  in
  loop list []
;;

let rec flatten =
  let open Nested_list in
  function
  | Atom x -> [ x ]
  | List xs -> concat_map xs ~f:flatten
;;

let p07 = flatten

let remove_consecutive_duplicates list ~equal =
  match list with
  | []
  | [ _ ] -> list
  | prev :: list ->
    let rec loop list prev accum =
      match list with
      | [] -> rev accum
      | hd :: tl
        when equal prev hd -> loop tl hd accum
      | hd :: tl -> loop tl hd (hd :: accum)
    in
    loop list prev [ prev ]
;;

let p08 = remove_consecutive_duplicates

let group_consecutive_duplicates list ~equal =
  match list with
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | prev :: list ->
    let rec loop list prev group accum =
      match list with
      | [] -> rev (group :: accum)
      | hd :: tl
        when equal prev hd -> loop tl hd (hd :: group) accum
      | hd :: tl -> loop tl hd [ hd ] (group :: accum)
    in
    loop list prev [ prev ] []
;;

let p09 = group_consecutive_duplicates

let run_length_encode list ~equal =
  group_consecutive_duplicates list ~equal
  |> List.map ~f:(fun xs -> List.length xs, List.hd_exn xs)
;;

let p10 = run_length_encode

module Private = struct
  let concat_map = concat_map
  let run_length_encode = run_length_encode
end
