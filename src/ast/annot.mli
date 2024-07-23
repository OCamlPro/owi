open Fmt

type annot =
  { annotid : string
  ; items : item list
  }

and item =
  | Atom of string
  | String of string
  | Id of string
  | Int of string
  | Float of string
  | Parens of item list
  | Annot of annot

val pp_annot : formatter -> annot -> unit

val record_annot : annot -> unit

val get_annots : ?name:string -> unit -> annot list
