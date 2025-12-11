(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Union_find = Union_find.Make (Smtml.Symbol)

type t = Smtml.Expr.Set.t Union_find.t

let empty : t = Union_find.empty

let add_one (condition : Smtml.Expr.t) pc : t =
  match Smtml.Expr.get_symbols [ condition ] with
  | hd :: tl ->
    (* We add the first symbol to the UF *)
    let pc =
      let c = Smtml.Expr.Set.singleton condition in
      Union_find.add ~merge:Smtml.Expr.Set.union hd c pc
    in
    (* We union-ize all symbols together, starting with the first one that has already been added *)
    let pc, _last_sym =
      List.fold_left
        (fun (pc, last_sym) sym ->
          (Union_find.union ~merge:Smtml.Expr.Set.union last_sym sym pc, sym) )
        (pc, hd) tl
    in
    pc
  | [] ->
    (* It means smtml did not properly simplified an expression! *)
    assert false

let add (condition : Symbolic_boolean.t) (pc : t) : t =
  (* we start by splitting the condition ((P & Q) & R) into a set {P; Q; R} before adding each of P, Q and R into the UF data structure, this way we maximize the independence of the PC *)
  let condition = Symbolic_boolean.to_expr condition in
  let splitted_condition = Smtml.Expr.split_conjunctions condition in
  Smtml.Expr.Set.fold add_one splitted_condition pc

(* Get all sub conditions of the path condition as a list of independent sets of constraints. *)
let slice pc = Union_find.explode pc

(* Return the set of constraints from [pc] that are relevant for [sym]. *)
let slice_on_symbol (sym : Smtml.Symbol.t) pc : Smtml.Expr.Set.t =
  match Union_find.find_opt sym pc with
  | Some s -> s
  | None ->
    (* if there is a symbol, it should have been added to the union-find structure before, otherwise it means `add` has not been called properly before *)
    assert false

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice_on_condition (c : Symbolic_boolean.t) pc : Smtml.Expr.Set.t =
  let c = Symbolic_boolean.to_expr c in
  match Smtml.Expr.get_symbols [ c ] with
  | sym0 :: _tl ->
    (* we need only the first symbol as all the others should have been merged with it *)
    slice_on_symbol sym0 pc
  | [] ->
    (* It means smtml did not properly simplified a expression! *)
    assert false
