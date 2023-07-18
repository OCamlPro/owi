
module Solver = Encoding.Batch.Make (Encoding.Z3_mappings)

type t =
  { solver : Solver.t
  ; pc : Sym_value.S.vbool list
  ; mem : Sym_memory.M.t
  }

let solver t = t.solver
let pc t = t.pc
let mem t = t.mem
