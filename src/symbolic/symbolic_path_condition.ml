(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Union_find = Union_find.Make (Smtml.Symbol)

type union_find = Smtml.Expr.Set.t Union_find.t

module Equalities : sig
  type t = Smtml.Value.t Smtml.Symbol.Map.t

  val to_expr_list : t -> Smtml.Expr.t list

  val to_set : t -> Smtml.Expr.Set.t
end = struct
  type t = Smtml.Value.t Smtml.Symbol.Map.t

  let to_expr sym value =
    Smtml.Expr.Bool.equal (Smtml.Expr.symbol sym) (Smtml.Expr.value value)

  let to_expr_list (equalities : t) : Smtml.Expr.t list =
    Smtml.Symbol.Map.bindings equalities
    |> List.map (fun (sym, expr) -> to_expr sym expr)

  let to_set (equalities : t) : Smtml.Expr.Set.t =
    Smtml.Symbol.Map.fold
      (fun sym expr set ->
        let e = to_expr sym expr in
        Smtml.Expr.Set.add e set )
      equalities Smtml.Expr.Set.empty
end

type t =
  { union_find : union_find
      (** the union find contains many partitions of the whole path condition,
          each partition is a set of expressions representing a conjunctions of
          transitively related formula, that is, if we have a formula xRy and
          yRz, then x, y and z end-up in the same partition. We can quickly
          merge two partition and find the partition to which a symbol belongs.
      *)
  ; equalities : Equalities.t
      (** the equalities is a map of symbol to known non-symbolic values *)
  ; is_unsat : bool  (** we learned that the PC is unsat *)
  }

let empty : t =
  let union_find = Union_find.empty in
  let equalities = Smtml.Symbol.Map.empty in
  let is_unsat = false in
  { union_find; equalities; is_unsat }

let add_one (condition : Smtml.Expr.t) (pc : t) : t =
  let condition = Smtml.Expr.inline_symbol_values pc.equalities condition in
  let condition = Smtml.Expr.simplify condition in
  match Smtml.Expr.get_symbols [ condition ] with
  | hd :: tl ->
    (* We add the first symbol to the union-find *)
    let union_find =
      let c = Smtml.Expr.Set.singleton condition in
      Union_find.add ~merge:Smtml.Expr.Set.union hd c pc.union_find
    in
    (* We union-ize all symbols together, starting with the first one that has already been added *)
    let union_find, _last_sym =
      List.fold_left
        (fun (pc, last_sym) sym ->
          (Union_find.union ~merge:Smtml.Expr.Set.union last_sym sym pc, sym) )
        (union_find, hd) tl
    in
    { pc with union_find }
  | [] -> (
    (* we managed to fully simplify the expression, *)
    match Smtml.Expr.view condition with
    | Smtml.Expr.Val True ->
      (* no need to change anything, the condition is a tautology *)
      pc
    | Val False ->
      (* the PC is unsat *)
      { pc with is_unsat = true }
    | _ ->
      (* it is a condition, it should be a boolean! *)
      assert false )

let add (condition : Symbolic_boolean.t) (pc : t) : t =
  (* we start by splitting the condition ((P & Q) & R) into a set {P; Q; R} before adding each of P, Q and R into the union_find data structure, this way we maximize the independence of the PC *)
  let splitted_condition = Smtml.Typed.Bool.split_conjunctions condition in
  let splitted_condition =
    Smtml.Expr.Set.map Smtml.Expr.simplify splitted_condition
  in
  let is_unsat = ref pc.is_unsat in
  let equalities =
    Smtml.Expr.Set.fold
      (fun (condition : Smtml.Expr.t) equalities ->
        match Smtml.Expr.view condition with
        (* if the condition if of the form e1 = e2 *)
        | Relop (_, Smtml.Ty.Relop.Eq, e1, e2) -> begin
          match (Smtml.Expr.view e1, Smtml.Expr.view e2) with
          (* if it is of the form symbol s = value v *)
          | Smtml.Expr.Symbol s, Val v | Val v, Symbol s -> begin
            match Smtml.Symbol.Map.find_opt s equalities with
            | None ->
              (* we don't already have an equality for the symbol s, so we add s = v it to the list *)
              Smtml.Symbol.Map.add s v equalities
            | Some v' ->
              if not (Smtml.Value.equal v v') then
                (* we have a symbol that is equal to two distinct values
                     thus the whole PC is unsat *)
                is_unsat := true;
              (* if the values were equal, no need to change anything
                   if they're not the same, we don't add it because we can have only one equality per symbol *)
              equalities
          end
          | _ -> equalities
        end
        | _ -> equalities )
      splitted_condition pc.equalities
  in
  let is_unsat = !is_unsat in
  let pc = { pc with equalities; is_unsat } in
  Smtml.Expr.Set.fold add_one splitted_condition pc

let filter_set (set : Smtml.Expr.Set.t) : Smtml.Expr.Set.t =
  Smtml.Expr.Set.fold
    (fun expr set ->
      match Smtml.Expr.view expr with
      | Val True -> set
      | _ -> Smtml.Expr.Set.add expr set )
    set Smtml.Expr.Set.empty

(* Get all sub conditions of the path condition as a list of independent sets of constraints. *)
let slice (pc : t) =
  if pc.is_unsat then
    [ Smtml.Expr.Set.singleton (Smtml.Expr.value Smtml.Value.False) ]
  else
    let slice = Union_find.explode pc.union_find |> List.map filter_set in
    let equalities =
      Equalities.to_expr_list pc.equalities |> List.map Smtml.Expr.Set.singleton
    in
    slice @ equalities

(* Return the set of constraints from [pc] that are relevant for [sym]. *)
let slice_on_symbol (sym : Smtml.Symbol.t) (pc : t) : Smtml.Expr.Set.t =
  if pc.is_unsat then
    Smtml.Expr.Set.singleton (Smtml.Expr.value Smtml.Value.False)
  else
    match Union_find.find_opt sym pc.union_find with
    | Some set ->
      let set = filter_set set in
      let equalities = Equalities.to_set pc.equalities in
      Smtml.Expr.Set.union set equalities
    | None ->
      (* The PC is sat no matter what *)
      Smtml.Expr.Set.empty

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice_on_condition (c : Symbolic_boolean.t) (pc : t) : Smtml.Expr.Set.t =
  match Smtml.Typed.get_symbols [ c ] with
  | sym0 :: _tl ->
    (* we need only the first symbol as all the others should have been merged with it *)
    slice_on_symbol sym0 pc
  | [] ->
    (* It means smtml did not properly simplified a expression! *)
    assert false
