(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a solver_module = (module Smtml.Solver_intf.S with type t = 'a)

type t = S : ('a solver_module * 'a) -> t [@@unboxed]

let instances = Atomic.make []

let rec add_solver solver =
  let l = Atomic.get instances in
  let success = Atomic.compare_and_set instances l (solver :: l) in
  if not success then add_solver solver

let fresh solver_ty () =
  let module Mapping = (val Smtml.Solver_dispatcher.mappings_of_solver solver_ty)
  in
  let module Mapping = Mapping.Fresh.Make () in
  let module Batch = Smtml.Solver.Cached (Mapping) in
  let solver_inst = Batch.create ~logic:QF_BVFP () in
  let solver = S ((module Batch), solver_inst) in
  if Log.is_bench_enabled () then add_solver solver;
  solver

let check (S (solver_module, s)) pc =
  let module Solver = (val solver_module) in
  Solver.check_set s pc

let model_of_partition (S (solver_module, s)) ~partition : Smtml.Model.t =
  let module Solver = (val solver_module) in
  let partition =
    List.map
      (fun pc ->
        match Solver.get_sat_model s pc with
        | `Model model -> model
        | `Unknown -> assert false
        | `Unsat -> assert false )
      partition
  in
  let model = Hashtbl.create 64 in
  List.iter
    (fun tbl -> Hashtbl.iter (fun sym v -> Hashtbl.add model sym v) tbl)
    partition;
  model

let get_sat_model (S (solver_module, s)) ~symbol_scopes ~pc =
  let module Solver = (val solver_module) in
  let symbols = Symbol_scope.only_symbols symbol_scopes in
  Solver.get_sat_model ~symbols s pc

let empty_stats = Smtml.Statistics.Map.empty

let stats_are_empty = Smtml.Statistics.Map.is_empty

let interrupt_all () =
  let solvers = Atomic.get instances in
  List.iter
    (fun (S (solver_module, s)) ->
      let module Solver = (val solver_module) in
      Solver.interrupt s )
    solvers

let get_all_stats () =
  if not (Log.is_bench_enabled ()) then empty_stats
  else begin
    let solvers = Atomic.get instances in
    List.fold_left
      (fun stats_acc (S (solver_module, s)) ->
        let module Solver = (val solver_module) in
        let stats =
          try Solver.get_statistics s
          with Z3.Error _ ->
            Logs.warn (fun m ->
              m
                "could not fetch the statistics of one solver because it was \
                 canceled" );
            empty_stats
        in
        Smtml.Statistics.merge stats stats_acc )
      empty_stats solvers
  end

let pp_stats = Smtml.Statistics.pp
