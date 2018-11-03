open! Base

type 'a t =
  | Atom of 'a
  | List of 'a t list
