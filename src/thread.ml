(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module Solver = Encoding.Batch.Make (Encoding.Z3_mappings)

type 'a solver_module = (module Encoding.Solver_intf.S with type t = 'a)

type solver = S : 'a solver_module * 'a -> solver

type t =
  { solver : solver
  ; pc : Sym_value.S.vbool list
  ; memories : Sym_memory.memories
  ; tables : Sym_table.tables
  ; globals : Sym_global.globals
  }

let solver t = t.solver

let pc t = t.pc

let memories t = t.memories

let tables t = t.tables

let globals t = t.globals

let solver_mod : Solver.t solver_module = (module Solver)

let create () =
  let solver = S (solver_mod, (Solver.create ())) in
  { solver
  ; pc = []
  ; memories = Sym_memory.init ()
  ; tables = Sym_table.init ()
  ; globals = Sym_global.init ()
  }
