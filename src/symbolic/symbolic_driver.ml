(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

let print_and_count_failures ~format ~out_file ~no_value
  ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure
  ~count_acc ~results ~with_breadcrumbs =
  let test_suite_dir = Fpath.(workspace / "test-suite") in
  let* (_created : bool) =
    if not no_value then OS.Dir.create test_suite_dir else Ok false
  in

  let rec aux count_acc results =
    match results () with
    | Seq.Nil -> Ok count_acc
    | Seq.Cons ((result, _thread), tl) ->
      let* model =
        match result with
        | (`EAssert (_, model, _, _, _) | `ETrap (_, model, _, _, _)) as bug ->
          let+ () =
            Model.print ~format ~out_file ~id:count_acc ~no_value
              ~no_stop_at_failure ~no_assert_failure_expression_printing
              ~with_breadcrumbs bug
          in
          model
      in
      let count_acc = succ count_acc in
      let* () =
        if not no_value then
          let testcase = Smtml.Model.get_bindings model |> List.map snd in
          Cmd_utils.write_testcase ~dir:test_suite_dir testcase
        else Ok ()
      in
      if no_stop_at_failure then aux count_acc tl else Ok count_acc
  in
  aux count_acc results

let sort_results deterministic_result_order results =
  if deterministic_result_order then
    results
    |> Seq.map (function (_, thread) as x ->
      (x, List.rev @@ Thread_with_memory.breadcrumbs thread) )
    |> List.of_seq
    |> List.sort (fun (_, bc1) (_, bc2) -> List.compare compare bc1 bc2)
    |> List.to_seq |> Seq.map fst
  else results

let mk_callback no_stop_at_failure fail_mode res_stack path_count =
 fun ~close_work_queue v ->
  let open Symbolic_choice_intf in
  Atomic.incr path_count;
  match (fail_mode, v) with
  | _, (EVal (), _) -> ()
  | ( (Symbolic_parameters.Both | Trap_only)
    , (ETrap (t, m, labels, breadcrumbs, symbol_scopes), thread) ) ->
    Ws.push
      (`ETrap (t, m, labels, breadcrumbs, symbol_scopes), thread)
      Prio.default res_stack;
    if not no_stop_at_failure then begin
      close_work_queue ()
    end
  | ( (Both | Assertion_only)
    , (EAssert (e, m, labels, breadcrumbs, symbol_scopes), thread) ) ->
    Ws.push
      (`EAssert (e, m, labels, breadcrumbs, symbol_scopes), thread)
      Prio.default res_stack;
    if not no_stop_at_failure then begin
      close_work_queue ()
    end
  | (Trap_only | Assertion_only), _ -> ()

let handle_result ~exploration_strategy ~workers ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~workspace ~solver ~model_format ~model_out_file ~with_breadcrumbs ~run_time
  (result : unit Symbolic_choice_with_memory.t) =
  let thread = Thread_with_memory.init () in
  let res_stack = Ws.make () in
  let path_count = Atomic.make 0 in
  let at_worker_value =
    mk_callback no_stop_at_failure fail_mode res_stack path_count
  in
  let time_before = (Unix.times ()).tms_utime in
  let domains : unit DomainPC.t Array.t =
    Symbolic_choice_with_memory.run exploration_strategy ~workers solver result
      thread ~at_worker_value
      ~at_worker_init:(fun () -> Ws.new_pledge res_stack)
      ~at_worker_end:(fun () -> Ws.end_pledge res_stack)
  in
  let results = Ws.read_as_seq res_stack ~finalizer:Fun.id in
  let results = sort_results deterministic_result_order results in
  let* count =
    print_and_count_failures ~format:model_format ~out_file:model_out_file
      ~no_value ~no_assert_failure_expression_printing ~workspace
      ~no_stop_at_failure ~count_acc:0 ~results ~with_breadcrumbs
  in

  (* We don't want to wait for domain to complete in normal/quiet mode because it may take quite some time (if a solver is running a long query, the interpreter is in a long concrete loop, or if the work queue was not correctly closed for instance) *)
  let wait_for_all_domains () =
    Array.iter
      (fun domain ->
        try DomainPC.join domain with
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
    let bench_stats = Thread_with_memory.bench_stats thread in
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
