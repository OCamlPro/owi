open Syntax
open Sexp

let name = "custom"

type prop = string

type t =
  { func : string
  ; preconditions : prop list
  ; postconditions : prop list
  }

let parse_prop _ = ""

let parse_func = function
  | List (Atom func :: items) -> Ok (func, items)
  | _ -> Error `Unknown_annotation_object

let parse_clause (preconditions, postconditions) = function
  | List (Atom clauseid :: rest) -> (
    match clauseid with
    | "requires" -> Ok (parse_prop rest :: preconditions, postconditions)
    | "ensures" -> Ok (preconditions, parse_prop rest :: postconditions)
    | _ -> Error `Unknown_annotation_clause )
  | _ -> Error `Unknown_annotation_clause

let parse_clauses = list_fold_left parse_clause ([], [])

let parse Annot.{ annotid; items } =
  if not (String.equal annotid name) then
    Error (`Annotation_id_incorrect annotid)
  else
    let* func, items' = parse_func items in
    let* preconditions, postconditions = parse_clauses items' in
    Ok { func; preconditions; postconditions }
