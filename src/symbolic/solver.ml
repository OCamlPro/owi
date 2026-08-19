(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a solver_module = (module Smtml.Solver_intf.S with type t = 'a)
type t = S : ('a solver_module * 'a) -> t [@@unboxed]

let instances = Atomic.make []

let add_solver solver =
  Multicore.atomic_modify (fun instances -> solver :: instances) instances

let fresh solver () =
  let module Mapping = (val Smtml.Solver_dispatcher.mappings_of_solver solver) in
  let module Mapping = Mapping.Fresh.Make () in
  let module Batch = Smtml.Solver.Cached (Mapping) in
  let solver_value = Batch.create ~logic:QF_BVFP () in
  let packed = S ((module Batch : Smtml.Solver_intf.S with type t = Batch.t), solver_value) in
  add_solver packed;
  packed

let solver_to_use = ref None

let dls_key =
  Domain.DLS.new_key (fun () ->
    match !solver_to_use with
    | Some solver_to_use -> fresh solver_to_use ()
    | None -> assert false)

let[@inline] get_current () = Domain.DLS.get dls_key

let cache = Smtml.Cache.Strong.create 64
let cache_mutex = Mutex.create ()

let check pc condition =
  Logs.info (fun m -> m "solver.check called");

  let query = Smtml.Expr.Set.add (Smtml.Typed.Unsafe.unwrap condition) pc in

  let query_expr =
    let es = Smtml.Expr.Set.to_list query in
    match es with
    | [] -> assert false
    | [e] -> e
    | e1 :: rest ->
        List.fold_left (fun acc e -> Smtml.Expr.Bool.and_ acc e) e1 rest
  in

  (* 1. Check unsat cache *)
  let cached_unsat =
    match Unsat_cache_control.get () with
    | Some cache ->
        Logs.debug (fun m -> m "Unsat cache: enabled, looking up");
        Unsat_cache.lookup cache query_expr
    | None ->
        Logs.debug (fun m -> m "Unsat cache: disabled");
        None
  in
  match cached_unsat with
  | Some _core ->
      Logs.debug (fun m -> m "Unsat cache: HIT, returning Unsat");
      `Unsat
  | None ->
      Logs.debug (fun m -> m "Unsat cache: MISS, proceeding to solver");
      (* 2. Existing exact-match cache *)
      let cached =
        Mutex.protect cache_mutex (fun () ->
          match Smtml.Cache.Strong.find_opt cache query with
          | Some sat -> Some (sat :> [ `Sat | `Unknown | `Unsat ])
          | None ->
              let neg_query =
                let neg_condition = Smtml.Typed.Bool.not condition in
                Smtml.Expr.Set.add (Smtml.Typed.Unsafe.unwrap neg_condition) pc
              in
              match Smtml.Cache.Strong.find_opt cache neg_query with
              | Some `Unsat -> Some (`Unsat :> [ `Sat | `Unknown | `Unsat ])
              | Some `Sat -> Some (`Sat :> [ `Sat | `Unknown | `Unsat ])
              | None -> None)
      in
      match cached with
      | Some sat ->
          sat
      | None ->
          let (S (solver_module, solver)) = get_current () in
          let module Solver = (val solver_module) in
          let sat = Solver.check_set solver query in

          let () =
            match sat with
            | `Unsat ->
                let fp = Hash_footprint.of_expr query_expr in
                Logs.debug (fun m -> m "Unsat cache: solver returned Unsat, storing fp=%d" (Hash_footprint.hash fp));
                (match Unsat_cache_control.get () with
                | Some cache ->
                    Unsat_cache.add cache fp [query_expr];
                    Logs.debug (fun m -> m "Unsat cache: store successful")
                | None -> ())
            | `Sat ->
                Logs.debug (fun m -> m "solver returned Sat, not storing")
            | `Unknown ->
                Logs.debug (fun m -> m "solver returned Unknown, not storing")
          in

          (* Store in exact cache if Sat or Unsat *)
          let narrow_for_cache (v : [ `Sat | `Unknown | `Unsat ]) : [ `Sat | `Unsat ] option =
            match v with
            | `Sat -> Some `Sat
            | `Unsat -> Some `Unsat
            | `Unknown -> None
          in
          (match narrow_for_cache sat with
          | Some v ->
              let _ = Mutex.protect cache_mutex (fun () -> Smtml.Cache.Strong.add cache query v) in ()
          | None -> ());
          sat

let store_unsat_formula formula =
  let fp = Hash_footprint.of_expr formula in
  match Unsat_cache_control.get () with
  | Some cache ->
      Unsat_cache.add cache fp [formula];
      Logs.debug (fun m -> m "Unsat cache: stored pruned path fp=%d" (Hash_footprint.hash fp))
  | None -> ()

let model_of_path_condition ~path_condition : Smtml.Model.t option =
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
          | `Unknown -> raise Unknown
          | `Unsat -> assert false)
        sub_conditions
    in
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
      Solver.interrupt s)
    solvers

let get_all_stats ~wait_for_all_domains =
  if not (Log.is_bench_enabled ()) then empty_stats
  else begin
    interrupt_all ();
    if Log.is_debug_enabled () then
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
                m "could not fetch the statistics of one solver because it was canceled, used empty stats instead");
              empty_stats
          in
          Smtml.Statistics.merge stats stats_acc)
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
        |> Smtml.Statistics.Map.add "cache hits ratio" (`Float hits_ratio))
  end

let pp_stats = Smtml.Statistics.pp

let was_interrupted () =
  let (S (solver_module, s)) = get_current () in
  let module Solver = (val solver_module) in
  Solver.was_interrupted s