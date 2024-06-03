(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Z3Batch = Smtml.Solver.Batch (Smtml.Z3_mappings)

type 'a solver_module = (module Smtml.Solver_intf.S with type t = 'a)

type t = S : ('a solver_module * 'a) -> t [@@unboxed]

let fresh () =
  let module Mapping = Smtml.Z3_mappings.Fresh.Make () in
  let module Batch = Smtml.Solver.Batch (Mapping) in
  let solver = Batch.create ~logic:QF_BVFP () in
  S ((module Batch), solver)

let check (S (solver_module, s)) pc =
  let module Solver = (val solver_module) in
  Solver.check s pc

let model (S (solver_module, s)) ~symbols =
  let module Solver = (val solver_module) in
  Solver.model ~symbols s
