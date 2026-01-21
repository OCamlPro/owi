(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a solver_module = (module Smtml.Solver_intf.S with type t = 'a)

type t = S : ('a solver_module * 'a) -> t [@@unboxed]

let instances = Atomic.make []

let add_solver solver =
  Multicore.atomic_modify (fun instances -> solver :: instances) instances

let fresh solver_ty () =
  let module Mapping = (val Smtml.Solver_dispatcher.mappings_of_solver solver_ty)
  in
  let module Mapping = Mapping.Fresh.Make () in
  let module Batch = Smtml.Solver.Batch (Mapping) in
  let solver_inst = Batch.create ~logic:QF_BVFP () in
  let solver = S ((module Batch), solver_inst) in
  if Log.is_bench_enabled () then add_solver solver;
  solver

let cache = Smtml.Cache.Strong.create 64

let cache_mutex = Mutex.create ()

let check (S (solver_module, s)) pc =
  let cached =
    Mutex.protect cache_mutex (fun () -> Smtml.Cache.Strong.find_opt cache pc)
  in
  match cached with
  | Some sat -> sat
  | None ->
    let module Solver = (val solver_module) in
    let sat = Solver.check_set s pc in
    Mutex.protect cache_mutex (fun () ->
      Smtml.Cache.Strong.replace cache pc sat );
    sat

let model_of_path_condition (S (solver_module, s)) ~path_condition :
  Smtml.Model.t Option.t =
  let exception Unknown in
  let module Solver = (val solver_module) in
  try
    let sub_conditions = Symbolic_path_condition.slice path_condition in
    let models =
      List.map
        (fun pc ->
          match Solver.get_sat_model s pc with
          | `Model model -> model
          | `Unknown ->
            (* it can happen if the solver is interrupted, otherwise it is an error, we raise, so the function can return an option that will be handled by the called *)
            raise Unknown
          | `Unsat ->
            (* it can not happen otherwise it means we reached an unreachable branch (or added garbage to the PC and did something wrong, who knows... :-) *)
            assert false )
        sub_conditions
    in
    (* We build the new complete model by merging all "sub models" *)
    let model = Hashtbl.create 64 in
    List.iter (Hashtbl.iter (Hashtbl.add model)) models;
    Some model
  with Unknown -> None

let model_of_set (S (solver_module, s)) ~symbol_scopes ~set =
  let module Solver = (val solver_module) in
  let symbols = Symbol_scope.only_symbols symbol_scopes in
  Solver.get_sat_model ~symbols s set

let empty_stats = Smtml.Statistics.Map.empty

let stats_are_empty = Smtml.Statistics.Map.is_empty

let interrupt_all () =
  let solvers = Atomic.get instances in
  List.iter
    (fun (S (solver_module, s)) ->
      let module Solver = (val solver_module) in
      Solver.interrupt s )
    solvers

let get_all_stats ~wait_for_all_domains =
  if not (Log.is_bench_enabled ()) then empty_stats
  else begin
    (* interrupt_all is unreliable but is a best effort to try to make sure we don't wait too long on really long requests.
     The reliable alternative would be to backup the statistics before each SMT request when in benchmark mode, but this would be too costly and lead to less accurate requests than random failures... *)
    interrupt_all ();
    if Log.is_debug_enabled () then
      (* we only do this in debug mode because otherwise it makes performances very bad *)
      wait_for_all_domains ();

    let solvers = Atomic.get instances in
    let stats =
      List.fold_left
        (fun stats_acc (S (solver_module, s)) ->
          let module Solver = (val solver_module) in
          let stats =
            try Solver.get_statistics s
            with Z3.Error _ ->
              Logs.warn (fun m ->
                m
                  "could not fetch the statistics of one solver because it was \
                   canceled, used empty stats instead" );
              empty_stats
          in
          Smtml.Statistics.merge stats stats_acc )
        empty_stats solvers
    in
    Mutex.protect cache_mutex (fun () ->
      let hits = Smtml.Cache.Strong.hits cache in
      let misses = Smtml.Cache.Strong.misses cache in
      let total = hits + misses in
      if total = 0 then stats
      else
        let hits_ratio =
          if hits = 0 then 0.
          else Float.of_int hits /. Float.of_int total *. 100.
        in
        Smtml.Statistics.Map.add "cache hits" (`Int hits) stats
        |> Smtml.Statistics.Map.add "cache misses" (`Int misses)
        |> Smtml.Statistics.Map.add "cache hits ratio" (`Float hits_ratio) )
  end

let pp_stats = Smtml.Statistics.pp
