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

(* TODO: should we make some normalization here? *)
(* TODO: it would be better to split the conjunctions in many sub-conditions *)
let add ({ uf; set } : t) (c : Symbolic_value.bool) : t =
  match Smtml.Expr.get_symbols [ c ] with
  | [] ->
    (* It means Smt.ml did not properly simplified a expression! *)
    Logs.err (fun m ->
      m "an expression was not simplified by smtml: %a" Symbolic_value.pp_int32
        c );
    assert false
  | hd :: tl ->
    let set = Smtml.Expr.Set.add c set in
    (* We add the first symbol to the UF *)
    let uf =
      let c = Smtml.Expr.Set.singleton c in
      Union_find.add ~merge:Smtml.Expr.Set.union hd c uf
    in
    (* We union-ize all symbols together, starting with the first one that has already been added *)
    let uf, _last_sym =
      List.fold_left
        (fun (uf, last_sym) sym ->
          (Union_find.union ~merge:Smtml.Expr.Set.union last_sym sym uf, sym) )
        (uf, hd) tl
    in
    { uf; set }

(* Turns all constraints into a set *)
let to_set { uf = _; set } = set

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice ({ uf; set = _ } : t) (c : Symbolic_value.bool) : Smtml.Expr.Set.t =
  match Smtml.Expr.get_symbols [ c ] with
  | [] ->
    (* It means Smt.ml did not properly simplified a expression... *)
    Logs.err (fun m ->
      m "an expression was not simplified by smtml: %a" Symbolic_value.pp_int32
        c );
    assert false
  | sym0 :: _tl -> (
    (* we need only the first symbol as all the others should have been merged with it *)
    match Union_find.find_opt sym0 uf with
    | None ->
      (* if there is a symbol, it should have been added to the union-find structure before, otherwise it means `add` has not been called properly before *)
      assert false
    | Some s -> s )
