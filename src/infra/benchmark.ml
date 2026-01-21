type stats =
  { solver_sat_time :
      Mtime.Span.t Atomic.t (* time taken by the solver to answer SAT queries *)
  ; solver_final_model_time : Mtime.Span.t Atomic.t
      (* time taken by the solver to answer final model queries at the end of execution when a bug is found *)
  ; solver_intermediate_model_time : Mtime.Span.t Atomic.t
      (* time taken by the solver to answer model queries during the execution, for instance when concretizing through `select_i32` *)
  ; path_count : int Atomic.t
  }

let empty_stats () =
  { solver_sat_time = Atomic.make Mtime.Span.min_span
  ; solver_final_model_time = Atomic.make Mtime.Span.min_span
  ; solver_intermediate_model_time = Atomic.make Mtime.Span.min_span
  ; path_count = Atomic.make 1
  }

let handle_time_span atomic_span f =
  if Log.is_bench_enabled () then (
    let counter = Mtime_clock.counter () in
    (* f is supposed to take a long time! *)
    let res = f () in
    let span = Mtime_clock.count counter in
    Multicore.atomic_modify (Mtime.Span.add span) atomic_span;
    res )
  else f ()

let with_utime f =
  if Log.is_bench_enabled () then
    let before = (Unix.times ()).tms_utime in
    let r = f () in
    let after = (Unix.times ()).tms_utime in
    (r, Some (after -. before))
  else (f (), None)

let percentage ~whole ~self =
  let whole = Mtime.Span.to_float_ns whole in
  let self = Mtime.Span.to_float_ns self in
  self /. whole *. 100.

let print_final ~bench_stats ~execution_time_a ~execution_time_b
  ~wait_for_all_domains =
  let execution_time =
    (* execution time shouldn't be none in bench mode *)
    let execution_time_a =
      match execution_time_a with None -> assert false | Some t -> t
    in
    (* they were both in seconds, we need to convert them to ns. *)
    let sum_ns = (execution_time_a +. execution_time_b) *. 1_000_000_000. in
    match Mtime.Span.of_float_ns sum_ns with
    | Some s -> s
    | None ->
      Log.warn (fun m -> m "Invalid time benchmarked!");
      Mtime.Span.zero
  in
  let solver_sat_time = Atomic.get bench_stats.solver_sat_time in
  let solver_final_model_time =
    Atomic.get bench_stats.solver_final_model_time
  in
  let solver_intermediate_model_time =
    Atomic.get bench_stats.solver_intermediate_model_time
  in
  let solver_model_time =
    Mtime.Span.add solver_intermediate_model_time solver_final_model_time
  in
  let solver_time = Mtime.Span.add solver_sat_time solver_model_time in
  let interpreter_time = Mtime.Span.abs_diff execution_time solver_time in

  Log.bench (fun m ->
    m "whole execution time          : %a" Mtime.Span.pp execution_time );

  Log.bench (fun m ->
    let percentage = percentage ~whole:execution_time ~self:solver_time in
    m "solver time                   : %a (%.2G%%)" Mtime.Span.pp solver_time
      percentage );
  Log.bench (fun m ->
    let percentage = percentage ~whole:execution_time ~self:solver_sat_time in
    m "solver SAT time               : %a (%.2G%%)" Mtime.Span.pp
      solver_sat_time percentage );
  Log.bench (fun m ->
    let percentage = percentage ~whole:execution_time ~self:solver_model_time in
    m "solver model time             : %a (%.2G%%)" Mtime.Span.pp
      solver_model_time percentage );
  Log.bench (fun m ->
    let percentage =
      percentage ~whole:execution_time ~self:solver_final_model_time
    in
    m "solver final model time       : %a (%.2G%%)" Mtime.Span.pp
      solver_final_model_time percentage );
  Log.bench (fun m ->
    let percentage =
      percentage ~whole:execution_time ~self:solver_intermediate_model_time
    in
    m "solver intermediate model time: %a (%.2G%%)" Mtime.Span.pp
      solver_intermediate_model_time percentage );

  Log.bench (fun m ->
    let percentage = percentage ~whole:execution_time ~self:interpreter_time in
    m "interpreter loop time         : %a (%.2G%%)" Mtime.Span.pp
      interpreter_time percentage );
  Log.bench (fun m -> m "path count: %d" (Atomic.get bench_stats.path_count));

  let solver_stats = Solver.get_all_stats ~wait_for_all_domains in
  Log.bench (fun m -> m "solver stats: %a" Solver.pp_stats solver_stats)
