(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax
module Expr = Smtml.Expr
module Choice = Symbolic_choice_with_memory

type fail_mode =
  | Trap_only
  | Assertion_only
  | Both

(* TODO: add a flag for this *)
let print_paths = false

let ( let*/ ) (t : 'a Result.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  match t with Error e -> Choice.return (Error e) | Ok x -> f x

let link_symbolic_modules link_state =
  let func_typ = Symbolic.Extern_func.extern_type in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic" ~func_typ
      Symbolic_wasm_ffi.symbolic_extern_module
  in
  Link.extern_module' link_state ~name:"summaries" ~func_typ
    Symbolic_wasm_ffi.summaries_extern_module

let run_file ~entry_point ~unsafe ~rac ~srac ~optimize ~invoke_with_symbols pc
  filename =
  let*/ m = Compile.File.until_binary_validate ~unsafe ~rac ~srac filename in
  let*/ m = Cmd_utils.set_entry_point entry_point invoke_with_symbols m in
  let link_state = link_symbolic_modules Link.empty_state in

  let*/ m, link_state =
    Compile.Binary.until_link ~unsafe ~optimize ~name:None link_state m
  in
  let m = Symbolic.convert_module_to_run m in
  let c = Interpret.Symbolic.modul link_state.envs m in
  Choice.bind pc (function Error _ as r -> Choice.return r | Ok () -> c)

let print_bug model_output_format no_value no_assert_failure_expression_printing
    =
  let to_string =
    match model_output_format with
    | Cmd_utils.Json -> Smtml.Model.to_json_string
    | Scfg -> Smtml.Model.to_scfg_string ~no_value
  in
  function
  | `ETrap (tr, model) ->
    Fmt.pr "Trap: %s@\n" (Trap.to_string tr);
    Fmt.pr "%s@\n" (to_string model)
  | `EAssert (assertion, model) ->
    if no_assert_failure_expression_printing then begin
      Fmt.pr "Assert failure@\n"
    end
    else begin
      Fmt.pr "Assert failure: %a@\n" Expr.pp assertion
    end;
    Fmt.pr "%s@\n" (to_string model)

let print_and_count_failures model_output_format no_value
  no_assert_failure_expression_printing workspace no_stop_at_failure count_acc
  results =
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
        | (`EAssert (_, model) | `ETrap (_, model)) as bug ->
          print_bug model_output_format no_value
            no_assert_failure_expression_printing bug;
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
    |> List.sort (fun (_, bc1) (_, bc2) ->
         List.compare Prelude.Int32.compare bc1 bc2 )
    |> List.to_seq |> Seq.map fst
  else results

let handle_result ~workers ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~workspace ~solver ~model_output_format result =
  let thread = Thread_with_memory.init () in
  let res_queue = Wq.make () in
  let path_count = Atomic.make 0 in
  let callback v =
    let open Symbolic_choice_intf in
    Atomic.incr path_count;
    match (fail_mode, v) with
    | _, (EVal (Ok ()), _) -> ()
    | _, (EVal (Error e), thread) -> Wq.push (`Error e, thread) res_queue
    | (Both | Trap_only), (ETrap (t, m), thread) ->
      Wq.push (`ETrap (t, m), thread) res_queue
    | (Both | Assertion_only), (EAssert (e, m), thread) ->
      Wq.push (`EAssert (e, m), thread) res_queue
    | (Trap_only | Assertion_only), _ -> ()
  in
  let join_handles =
    Symbolic_choice_with_memory.run ~workers solver result thread ~callback
      ~callback_init:(fun () -> Wq.make_pledge res_queue)
      ~callback_end:(fun () -> Wq.end_pledge res_queue)
  in
  let results =
    Wq.read_as_seq res_queue ~finalizer:(fun () ->
      Array.iter Domain.join join_handles )
  in
  let results = sort_results deterministic_result_order results in
  let* count =
    print_and_count_failures model_output_format no_value
      no_assert_failure_expression_printing workspace no_stop_at_failure 0
      results
  in
  if print_paths then Fmt.pr "Completed paths: %d@." (Atomic.get path_count);
  let+ () = if count > 0 then Error (`Found_bug count) else Ok () in
  Fmt.pr "All OK@."

(* NB: This function propagates potential errors (Result.err) occurring
   during evaluation (OS, syntax error, etc.), except for Trap and Assert,
   which are handled here. Most of the computations are done in the Result
   monad, hence the let*. *)
let cmd ~profiling ~debug ~unsafe ~rac ~srac ~optimize ~workers
  ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~workspace ~solver ~files ~profile
  ~model_output_format ~entry_point ~invoke_with_symbols =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_sym_%s"
  in

  Option.iter Stats.init_logger_to_file profile;
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  (* deterministic_result_order implies no_stop_at_failure *)
  let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in
  let pc = Choice.return (Ok ()) in
  let result =
    List.fold_left
      (run_file ~entry_point ~unsafe ~rac ~srac ~optimize ~invoke_with_symbols)
      pc files
  in
  handle_result ~fail_mode ~workers ~solver ~deterministic_result_order
    ~model_output_format ~no_value ~no_assert_failure_expression_printing
    ~workspace ~no_stop_at_failure result
