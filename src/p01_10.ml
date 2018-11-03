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
