type t =
  | Atom of string
  | List of t list

val pp_sexp : Format.formatter -> t -> unit
