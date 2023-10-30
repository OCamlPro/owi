(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module Solver = Encoding.Batch.Make (Encoding.Z3_mappings)

type 'a solver_module = (module Encoding.Solver_intf.S with type t = 'a)

type solver = S : 'a solver_module * 'a -> solver

type t =
  { solver : solver
  ; pc : Symbolic_value.S.vbool list
  ; memories : Symbolic_memory.memories
  ; tables : Symbolic_table.tables
  ; globals : Symbolic_global.globals
  ; sym_cnt : int ref
  }

let solver t = t.solver

let pc t = t.pc

let memories t = t.memories

let tables t = t.tables

let globals t = t.globals

let sym_cnt t = t.sym_cnt

let solver_mod : Solver.t solver_module = (module Solver)

let create () =
  let solver = S (solver_mod, Solver.create ()) in
  { solver
  ; pc = []
  ; memories = Symbolic_memory.init ()
  ; tables = Symbolic_table.init ()
  ; globals = Symbolic_global.init ()
  ; sym_cnt = ref 0
  }

let clone { solver; pc; memories; tables; globals; sym_cnt } =
  let memories = Symbolic_memory.clone memories in
  let tables = Symbolic_table.clone tables in
  let globals = Symbolic_global.clone globals in
  let sym_cnt = ref !sym_cnt in
  { solver; pc; memories; tables; globals; sym_cnt }
