open Fmt

type t =
  | Atom of string
  | List of t list

let rec pp_sexp fmt = function
  | Atom str -> pf fmt "%s" str
  | List [] -> pf fmt "()"
  | List (_ as l) ->
    pf fmt "@[<b>(%a)@]" (list ~sep:(fun fmt () -> pf fmt "@ ") pp_sexp) l
