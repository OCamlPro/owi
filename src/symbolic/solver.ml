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

let check =
  let module Cache = Smtml.Cache.Strong in
  let cache = Cache.create 512 in
  let lock = Mutex.create () in
  fun (S (solver_module, s)) pc ->
    Mutex.lock lock;
    let satisfiability = Cache.find_opt cache pc in
    Mutex.unlock lock;
    match satisfiability with
    | Some satisfiability -> satisfiability
    | None ->
      let module Solver = (val solver_module) in
      let satisfiability = Solver.check_set s pc in
      Mutex.lock lock;
      Cache.add cache pc satisfiability;
      Mutex.unlock lock;
      satisfiability

let model (S (solver_module, s)) ~symbols ~pc =
  let module Solver = (val solver_module) in
  let model =
    (* we don't memoïze this call, because check_set *must* be used before calling model *)
    match Solver.check_set s pc with
    | `Sat -> begin
      match Solver.model ?symbols s with
      | None -> assert false
      | Some model -> model
    end
    | `Unsat -> assert false
    | `Unknown -> assert false
  in
  model
