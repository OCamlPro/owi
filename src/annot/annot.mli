open Fmt
open Types

type t =
  { annotid : string
  ; items : Sexp.t
  }

type 'a annot =
  | Contract of 'a Contract.t
  | Annot of t

val pp_annot : formatter -> text annot -> unit

val record_annot : string -> text annot -> unit

val get_annots : ?name:string -> unit -> text annot list
