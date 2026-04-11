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

let solver_to_use = ref None

let dls_key =
  Domain.DLS.new_key (fun () ->
    let solver_to_use = !solver_to_use in
    match solver_to_use with
    | Some solver_to_use -> fresh solver_to_use ()
    | None -> assert false )

let[@inline] get_current () = Domain.DLS.get dls_key

let cache = Smtml.Cache.Strong.create 64

let cache_mutex = Mutex.create ()

let check pc condition =
  let query = Smtml.Expr.Set.add (Smtml.Typed.Unsafe.unwrap condition) pc in
  let cached =
    Mutex.protect cache_mutex (fun () ->
      match Smtml.Cache.Strong.find_opt cache query with
      | Some sat -> Some sat
      | None -> (
        let neg_query =
          let neg_condition = Smtml.Typed.Bool.not condition in
          Smtml.Expr.Set.add (Smtml.Typed.Unsafe.unwrap neg_condition) pc
        in
        match Smtml.Cache.Strong.find_opt cache neg_query with
        | Some `Unsat ->
          (* this is an optimisation under the assumption that the PC is always SAT (i.e. we are performing eager pruning), in such a case, when a branch is unsat, we don't have to check the reachability of the other's branch negation, because it is always going to be SAT. *)
          Some `Sat
        | None | Some _ ->
          (* we can't deduce anything *)
          None ) )
  in
  match cached with
  | Some sat -> sat
  | None ->
    (* there was nothing useful in the cache and we have to make the check for real! *)
    let (S (solver_module, s)) = get_current () in
    let module Solver = (val solver_module) in
    let sat = Solver.check_set s query in
    Mutex.protect cache_mutex (fun () ->
      (* using `add` is better than `replace` because it avoid comparing the set again which might be huge! *)
      Smtml.Cache.Strong.add cache query sat );
    sat

let model_of_path_condition ~path_condition : Smtml.Model.t Option.t =
  let exception Unknown in
  let (S (solver_module, s)) = get_current () in
  let module Solver = (val solver_module) in
  try
    let sub_conditions = Symex.Path_condition.to_list path_condition in
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

let model_of_set ~symbol_scopes ~set =
  let (S (solver_module, s)) = get_current () in
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

let was_interrupted () =
  let (S (solver_module, s)) = get_current () in
  let module Solver = (val solver_module) in
  Solver.was_interrupted s
