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
