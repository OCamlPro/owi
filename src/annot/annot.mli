open Fmt

type t =
  { annotid : string
  ; items : Sexp.t
  }

val pp_annot : formatter -> t -> unit

val record_annot : t -> unit

val get_annots : ?name:string -> unit -> t list
