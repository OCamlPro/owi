module Solver = Encoding.Batch.Make (Encoding.Z3_mappings)

type t =
  { solver : Solver.t
  ; pc : Sym_value.S.vbool list
  ; mem : Sym_memory.M.t
  ; tables : Sym_table.tables
  }

let solver t = t.solver

let pc t = t.pc

let mem t = t.mem

let tables t = t.tables

let create () =
  let solver = Solver.create () in
  { solver; pc = []; mem = Sym_memory.M.create 2l; tables = Sym_table.init () }
