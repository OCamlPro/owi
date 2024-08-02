open Fmt
open Types
open Spec

type 'a t =
  { func : 'a indice
  ; preconditions : 'a prop list
  ; postconditions : 'a prop list
  }

val pp_contract : formatter -> 'a t -> unit
