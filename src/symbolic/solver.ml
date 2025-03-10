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
  Stats.start_span "check" "solver";
  let module Solver = (val solver_module) in
  let check = Solver.check s pc in
  Stats.close_span ();
  check

let interrupt (S (solver_module, s)) =
  let module Solver = (val solver_module) in
  Solver.interrupt s

let model (S (solver_module, s)) ~symbols ~pc =
  Stats.start_span "model" "solver";
  let module Solver = (val solver_module) in
  let model =
    match Solver.check s pc with
    | `Sat -> begin
      match Solver.model ?symbols s with
      | None -> assert false
      | Some model -> model
    end
    | `Unsat | `Unknown -> assert false
  in
  Stats.close_span ();
  model
