(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Union_find = Union_find.Make (Smtml.Symbol)

type t = Smtml.Expr.Set.t Union_find.t

let empty : t = Union_find.empty

let add_one uf (condition : Symbolic_boolean.t) : t =
  match Smtml.Expr.get_symbols [ condition ] with
  | [] ->
    (* It means Smt.ml did not properly simplified a expression! *)
    Log.err (fun m ->
      m "an expression was not simplified by smtml: %a" Symbolic_boolean.pp
        condition );
    assert false
  | hd :: tl ->
    (* We add the first symbol to the UF *)
    let uf =
      let c = Smtml.Expr.Set.singleton condition in
      Union_find.add ~merge:Smtml.Expr.Set.union hd c uf
    in
    (* We union-ize all symbols together, starting with the first one that has already been added *)
    let uf, _last_sym =
      List.fold_left
        (fun (uf, last_sym) sym ->
          (Union_find.union ~merge:Smtml.Expr.Set.union last_sym sym uf, sym) )
        (uf, hd) tl
    in
    uf

let add (pc : t) (condition : Symbolic_boolean.t) : t =
  (* we start by splitting the condition ((P & Q) & R) into a set {P; Q; R} before adding each of P, Q and R into the UF data structure, this way we maximize the independence of the PC *)
  let splitted_condition = Smtml.Expr.split_conjunctions condition in
  Smtml.Expr.Set.fold
    (fun condition pc -> add_one pc condition)
    splitted_condition pc

(* Get all partitions of the union find as a list. *)
let explode uf = Union_find.explode uf

(* Return the set of constraints from [pc] that are relevant for [sym]. *)
let slice_on_symbol uf (sym : Smtml.Symbol.t) : Smtml.Expr.Set.t =
  match Union_find.find_opt sym uf with
  | None ->
    (* if there is a symbol, it should have been added to the union-find structure before, otherwise it means `add` has not been called properly before *)
    assert false
  | Some s -> s

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice pc (c : Symbolic_boolean.t) : Smtml.Expr.Set.t =
  match Smtml.Expr.get_symbols [ c ] with
  | [] ->
    (* It means Smt.ml did not properly simplified a expression... *)
    Log.err (fun m ->
      m "an expression was not simplified by smtml: %a" Symbolic_boolean.pp c );
    assert false
  | sym0 :: _tl ->
    (* we need only the first symbol as all the others should have been merged with it *)
    slice_on_symbol pc sym0
