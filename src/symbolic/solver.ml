(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a solver_module = (module Smtml.Solver_intf.S with type t = 'a)

type solver = S : ('a solver_module * 'a) -> solver [@@unboxed]

module Z3Batch = Smtml.Solver.Batch (Smtml.Z3_mappings)

let solver_mod : Z3Batch.t solver_module = (module Z3Batch)

let fresh_solver () =
  let module Mapping = Smtml.Z3_mappings.Fresh.Make () in
  let module Batch = Smtml.Solver.Batch (Mapping) in
  let solver = Batch.create ~logic:QF_BVFP () in
  S ((module Batch), solver)
