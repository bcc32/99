open! Base

let rec last_exn = function
  | [] -> raise_s [%message "empty list"]
  | [ x ] -> x
  | _ :: tl -> last_exn tl
;;

let p01 = last_exn
