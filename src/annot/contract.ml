open Types
open Fmt
open Spec
open Syntax

type 'a t =
  { funcid : 'a indice
  ; preconditions : 'a prop list
  ; postconditions : 'a prop list
  }

let pp_contract fmt { funcid; preconditions; postconditions } =
  pf fmt
    "@[<v>Contract of function %a@,\
     Preconditions:@;\
     <1 2>@[<v>%a@]@,\
     Postconditions:@;\
     <1 2>@[<v>%a@]@]" pp_indice funcid
    (list ~sep:pp_newline pp_prop)
    preconditions
    (list ~sep:pp_newline pp_prop)
    postconditions

let cons_first (l1, l2) x1 = (x1 :: l1, l2)

let cons_second (l1, l2) x2 = (l1, x2 :: l2)

let parse_contract =
  let open Sexp in
  function
  | List (Atom funcid :: conds) ->
    let aux acc = function
      | List [ Atom "requires"; precond ] ->
        let+ precond = parse_prop precond in
        cons_first acc precond
      | List [ Atom "ensures"; postcond ] ->
        let+ postcond = parse_prop postcond in
        cons_second acc postcond
      | _ as s -> Error (`Unknown_annotation_clause s)
    in
    let* funcid = parse_indice funcid in
    let+ preconditions, postconditions = list_fold_left aux ([], []) conds in
    { funcid; preconditions; postconditions }
  | _ as s -> Error (`Unknown_annotation_object s)
