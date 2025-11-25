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
        | (`EAssert (_, model, _, _, _, _) | `ETrap (_, model, _, _, _, _)) as
          bug ->
          let* () =
            Model.print ~format ~out_file ~id:count_acc ~no_value
              ~no_stop_at_failure ~no_assert_failure_expression_printing
              ~with_breadcrumbs bug
          in
          Ok model
        | `Error e -> Error e
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

let mk_callback fail_mode res_stack path_count =
 fun v ->
  let open Symbolic_choice_intf in
  Atomic.incr path_count;
  match (fail_mode, v) with
  | _, (EVal (), _) -> ()
  | ( (Symbolic_parameters.Both | Trap_only)
    , (ETrap (t, m, labels, breadcrumbs, symbol_scopes, stats), thread) ) ->
    Ws.push
      (`ETrap (t, m, labels, breadcrumbs, symbol_scopes, stats), thread)
      Prio.default res_stack
  | ( (Both | Assertion_only)
    , (EAssert (e, m, labels, breadcrumbs, symbol_scopes, stats), thread) ) ->
    Ws.push
      (`EAssert (e, m, labels, breadcrumbs, symbol_scopes, stats), thread)
      Prio.default res_stack
  | (Trap_only | Assertion_only), _ -> ()

let handle_result ~exploration_strategy ~workers ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~workspace ~solver ~model_format ~model_out_file ~with_breadcrumbs ~run_time
  (result : unit Symbolic.Choice.t) =
  let thread = Thread_with_memory.init () in
  let res_stack = Ws.make () in
  let path_count = Atomic.make 0 in
  let callback = mk_callback fail_mode res_stack path_count in
  let time_counter = Mtime_clock.counter () in
  let time_before = (Unix.times ()).tms_utime in
  let join_handles =
    Symbolic_choice_with_memory.run exploration_strategy ~workers solver result
      thread ~callback
      ~callback_init:(fun () -> Ws.make_pledge res_stack)
      ~callback_end:(fun () -> Ws.end_pledge res_stack)
  in
  let results =
    Ws.read_as_seq res_stack ~finalizer:(fun () ->
      Array.iter Domain.join join_handles )
  in
  let time_after = (Unix.times ()).tms_utime in
  let interpreter_time = time_after -. time_before in
  let results = sort_results deterministic_result_order results in

  let* count =
    print_and_count_failures ~format:model_format ~out_file:model_out_file
      ~no_value ~no_assert_failure_expression_printing ~workspace
      ~no_stop_at_failure ~count_acc:0 ~results ~with_breadcrumbs
  in

  Log.bench (fun m ->
    let time_counter = Mtime_clock.count time_counter in
    let bench_stats = Thread_with_memory.bench_stats thread in
    let solver_time = Atomic.get bench_stats.solver_time in
    let interpreter_time =
      let run_time = match run_time with None -> assert false | Some t -> t in
      (interpreter_time +. run_time) *. 1000.
    in
    (* run_time shouldn't be none in bench mode *)
    m
      "Benchmarks:@\n\
       execution time: %a@\n\
       @[<v>solver time: %a@;\
       interpreter time: %fms@;\
       @]"
      Mtime.Span.pp time_counter Mtime.Span.pp solver_time interpreter_time );

  Log.info (fun m -> m "Completed paths: %d" (Atomic.get path_count));

  if count > 0 then Error (`Found_bug count)
  else Ok (Log.app (fun m -> m "All OK!"))
