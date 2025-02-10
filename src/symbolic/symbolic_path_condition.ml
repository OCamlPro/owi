(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Union_find = Union_find.Make (Smtml.Symbol)

type t = Smtml.Expr.Set.t Union_find.t

let empty : t = Union_find.empty

let rec union_on_all_syms (c : Smtml.Expr.Set.t)
  (pc : Smtml.Expr.Set.t Union_find.t) = function
  | [] ->
    (* TODO: I'm not sure what to do here... *)
    pc
  | [ last_sym ] -> Union_find.add ~merge:Smtml.Expr.Set.union last_sym c pc
  | sym0 :: (sym1 :: _tl as tl) ->
    let pc = Union_find.union ~merge:Smtml.Expr.Set.union sym0 sym1 pc in
    union_on_all_syms c pc tl

(* TODO: should we make some normalization here? *)
let add (pc : t) (c : Symbolic_value.bool) : t =
  let symbols = Smtml.Expr.get_symbols [ c ] in
  let c = Smtml.Expr.Set.singleton c in
  union_on_all_syms c pc symbols

(* Turns all constraints into a set *)
let to_set pc =
  Union_find.merge_all_values ~empty:Smtml.Expr.Set.empty
    ~merge:Smtml.Expr.Set.union pc

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice (pc : t) (c : Symbolic_value.bool) : Smtml.Expr.Set.t =
  match Smtml.Expr.get_symbols [ c ] with
  | [] -> Smtml.Expr.Set.add c (to_set pc)
  | sym0 :: _tl -> (
    (* we need only the first symbol as all the other one should have been merged with it *)
    match Union_find.find_opt sym0 pc with
    | None ->
      (* if there is a symbol, it should have been added to the union-find structure before *)
      assert false
    | Some s -> s )
