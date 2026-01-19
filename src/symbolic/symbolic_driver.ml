(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax
module Bugs = Prio.Make (Prio.FIFO)

let print_and_count_bugs ~format ~out_file ~no_value
  ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure ~results
  ~with_breadcrumbs =
  let test_suite_dir = Fpath.(workspace / "test-suite") in
  let* (_created : bool) =
    if not no_value then OS.Dir.create test_suite_dir else Ok false
  in

  let rec aux count results =
    match results () with
    | Seq.Nil -> Ok count
    | Seq.Cons (bug, tl) ->
      let* () =
        Model.print ~format ~out_file ~id:count ~no_value ~no_stop_at_failure
          ~no_assert_failure_expression_printing ~with_breadcrumbs bug
      in
      let* () =
        if not no_value then
          let testcase = Smtml.Model.get_bindings bug.model |> List.map snd in
          Cmd_utils.write_testcase ~dir:test_suite_dir testcase
        else Ok ()
      in
      let count = succ count in
      if no_stop_at_failure then aux count tl else Ok count
  in
  aux 0 results

let mk_callback no_stop_at_failure fail_mode res_stack path_count =
 fun ~close_work_queue v ->
  Atomic.incr path_count;
  match v with
  | Ok (), _thread -> ()
  | Error bug, _thread ->
    let should_be_added =
      match fail_mode with
      | Symbolic_parameters.Both -> true
      | Assertion_only -> Bug.is_assertion bug
      | Trap_only -> Bug.is_trap bug
    in
    if should_be_added then begin
      Bugs.push bug Prio.dummy res_stack;
      if not no_stop_at_failure then begin
        close_work_queue ()
      end
    end

let handle_result ~exploration_strategy ~workers ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~workspace ~solver ~model_format ~model_out_file ~with_breadcrumbs ~run_time
  (result : unit Symbolic_choice.t) =
  let thread = Thread.init () in
  let bug_stack = Bugs.make () in
  let path_count = Atomic.make 0 in
  let at_worker_value =
    mk_callback no_stop_at_failure fail_mode bug_stack path_count
  in
  let time_before = (Unix.times ()).tms_utime in
  let domains : unit Domain.t Array.t =
    Symbolic_choice.run exploration_strategy ~workers solver result thread
      ~at_worker_value
      ~at_worker_init:(fun () -> Bugs.new_pledge bug_stack)
      ~at_worker_end:(fun () -> Bugs.end_pledge bug_stack)
  in
  let results =
    Bugs.read_as_seq bug_stack |> Bug.sort_seq_if deterministic_result_order
  in
  let* count =
    print_and_count_bugs ~format:model_format ~out_file:model_out_file ~no_value
      ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure
      ~results ~with_breadcrumbs
  in

  (* We don't want to wait for domain to complete in normal/quiet mode because it may take quite some time (if a solver is running a long query, the interpreter is in a long concrete loop, or if the work queue was not correctly closed for instance) *)
  let wait_for_all_domains () =
    Array.iter
      (fun domain ->
        try Domain.join domain with
        | Z3.Error msg ->
          Log.info (fun m ->
            m "one domain exited with the following Z3 exception: %s" msg )
        | exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          Log.info (fun m ->
            m
              "one domaine exited with the %s exception which was only noticed \
               while waiting for all domain to join:@\n\
              \  @[<v>%s@]"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string backtrace) ) )
      domains
  in

  if Log.is_bench_enabled () then begin
    let bench_stats = thread.bench_stats in
    let execution_time_b =
      let time_after = (Unix.times ()).tms_utime in
      time_after -. time_before
    in
    Benchmark.print_final ~bench_stats ~execution_time_a:run_time
      ~execution_time_b ~wait_for_all_domains
  end
  else if Log.is_debug_enabled () then begin
    (* we only do this in debug mode because otherwise it makes performances very bad *)
    wait_for_all_domains ()
  end;

  Log.info (fun m -> m "Completed paths: %d" (Atomic.get path_count));

  if count > 0 then Error (`Found_bug count)
  else Ok (Log.app (fun m -> m "All OK!"))
