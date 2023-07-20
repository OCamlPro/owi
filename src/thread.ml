module Solver = Encoding.Batch.Make (Encoding.Z3_mappings)

type t =
  { solver : Solver.t
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

let create () =
  let solver = Solver.create () in
  { solver
  ; pc = []
  ; memories = Sym_memory.init ()
  ; tables = Sym_table.init ()
  ; globals = Sym_global.init ()
  }
