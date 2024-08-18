(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Fmt
open Spec
open Syntax

type 'a t =
  { funcid : 'a indice
  ; preconditions : 'a prop list
  ; postconditions : 'a prop list
  }

let compare_funcid c1 c2 = compare_indice c1.funcid c2.funcid

let join_contract { preconditions = pre1; postconditions = post1; funcid }
  { preconditions = pre2; postconditions = post2; _ } =
  let preconditions = pre1 @ pre2 in
  let postconditions = post1 @ post2 in
  { funcid; preconditions; postconditions }

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

let parse_contract =
  let open Sexp in
  function
  | List (Atom funcid :: conds) ->
    let aux (l1, l2) = function
      | List [ Atom "requires"; precond ] ->
        let+ precond = parse_prop precond in
        (precond :: l1, l2)
      | List [ Atom "ensures"; postcond ] ->
        let+ postcond = parse_prop postcond in
        (l1, postcond :: l2)
      | cl -> Error (`Unknown_annotation_clause cl)
    in
    let* funcid = parse_indice funcid in
    let+ preconditions, postconditions = list_fold_left aux ([], []) conds in
    { funcid; preconditions; postconditions }
  | annot -> Error (`Unknown_annotation_object annot)
