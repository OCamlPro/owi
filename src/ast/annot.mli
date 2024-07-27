open Fmt

type annot =
  { annotid : string
  ; items : Sexp.t
  }

val pp_annot : formatter -> annot -> unit

val record_annot : annot -> unit

val get_annots : ?name:string -> unit -> annot list
