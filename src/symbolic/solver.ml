(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a solver_module = (module Smtml.Solver_intf.S with type t = 'a)

type t = S : ('a solver_module * 'a) -> t [@@unboxed]

let fresh solver () =
  let module Mapping = (val Smtml.Solver_dispatcher.mappings_of_solver solver)
  in
  let module Mapping = Mapping.Fresh.Make () in
  let module Batch = Smtml.Solver.Batch (Mapping) in
  let solver = Batch.create ~logic:QF_BVFP () in
  S ((module Batch), solver)

let check (S (solver_module, s)) pc =
  let module Solver = (val solver_module) in
  (* TODO: maybe we should change the API of smtml so that Solver.check expects a set instead of a list ? (to avoid duplicates) *)
  let pc = Smtml.Expr.Set.to_list pc in
  Solver.check s pc

let model (S (solver_module, s)) ~symbols ~pc =
  let module Solver = (val solver_module) in
  let pc = Smtml.Expr.Set.to_list pc in
  let model =
    match Solver.check s pc with
    | `Sat -> begin
      match Solver.model ?symbols s with
      | None -> assert false
      | Some model -> model
    end
    | `Unsat -> assert false
    | `Unknown -> assert false
  in
  model
