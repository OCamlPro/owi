(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Union_find = Union_find.Make (Smtml.Symbol)

type t =
  { uf : Smtml.Expr.Set.t Union_find.t
  ; set : Smtml.Expr.Set.t
  }

let empty : t =
  let uf = Union_find.empty in
  let set = Smtml.Expr.Set.empty in
  { uf; set }

let rec union_on_all_syms (c : Smtml.Expr.Set.t)
  (uf : Smtml.Expr.Set.t Union_find.t) = function
  | [] -> uf
  | [ last_sym ] -> Union_find.add ~merge:Smtml.Expr.Set.union last_sym c uf
  | sym0 :: (sym1 :: _tl as tl) ->
    let uf = Union_find.union ~merge:Smtml.Expr.Set.union sym0 sym1 uf in
    union_on_all_syms c uf tl

(* TODO: should we make some normalization here? *)
(* TODO: it would be better to split the conjunctions in many sub-conditions *)
let add ({ uf; set } : t) (c : Symbolic_value.bool) : t =
  let set = Smtml.Expr.Set.add c set in
  let symbols = Smtml.Expr.get_symbols [ c ] in
  let c = Smtml.Expr.Set.singleton c in
  let uf = union_on_all_syms c uf symbols in
  { uf; set }

(* Turns all constraints into a set *)
let to_set { uf = _; set } = set

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice ({ uf; set } : t) (c : Symbolic_value.bool) : Smtml.Expr.Set.t =
  match Smtml.Expr.get_symbols [ c ] with
  | [] ->
    (* TODO: It means Smt.ml did not properly simplified a expression... *)
    (* assert false *)
    (* For now, we use this, it should be removed and assert false should be used instead later *)
    set
  | sym0 :: _tl -> (
    (* we need only the first symbol as all the other one should have been merged with it *)
    match Union_find.find_opt sym0 uf with
    | None ->
      (* if there is a symbol, it should have been added to the union-find structure before, otherwise it means `add` has not been called properly before *)
      assert false
    | Some s -> s )
