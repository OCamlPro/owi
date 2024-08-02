open Fmt
open Types
open Spec

type 'a t =
  { func : 'a indice
  ; preconditions : 'a prop list
  ; postconditions : 'a prop list
  }

let pp_contract fmt { func; preconditions; postconditions } =
  pf fmt "%a@,%a@,%a@," pp_indice func
    (list ~sep:pp_newline pp_prop)
    preconditions
    (list ~sep:pp_newline pp_prop)
    postconditions
