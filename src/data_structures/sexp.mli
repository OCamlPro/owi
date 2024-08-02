open Fmt

type t =
  | Atom of string
  | List of t list

val pp_sexp : formatter -> t -> unit
