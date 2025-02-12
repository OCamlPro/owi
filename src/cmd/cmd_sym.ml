(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Expr = Smtml.Expr
module Choice = Symbolic_choice_with_memory

type fail_mode =
  [ `Trap_only
  | `Assertion_only
  | `Both
  ]

(* TODO: add a flag for this *)
let print_paths = false

let ( let*/ ) (t : 'a Result.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  match t with Error e -> Choice.return (Error e) | Ok x -> f x

let link_state =
  lazy
    (let func_typ = Symbolic.Extern_func.extern_type in
     let link_state =
       Link.extern_module' Link.empty_state ~name:"symbolic" ~func_typ
         Symbolic_wasm_ffi.symbolic_extern_module
     in
     Link.extern_module' link_state ~name:"summaries" ~func_typ
       Symbolic_wasm_ffi.summaries_extern_module )

let run_file ~unsafe ~rac ~srac ~optimize pc filename =
  let*/ m = Compile.File.until_binary_validate ~unsafe ~rac ~srac filename in
  let*/ m = Cmd_utils.add_main_as_start m in
  let link_state = Lazy.force link_state in

  let*/ m, link_state =
    Compile.Binary.until_link ~unsafe ~optimize ~name:None link_state m
  in
  let m = Symbolic.convert_module_to_run m in
  let c = Interpret.Symbolic.modul link_state.envs m in
  Choice.bind pc (fun r ->
    match r with Error _ -> Choice.return r | Ok () -> c )

(* NB: This function propagates potential errors (Result.err) occurring
   during evaluation (OS, syntax error, etc.), except for Trap and Assert,
   which are handled here. Most of the computations are done in the Result
   monad, hence the let*. *)
let cmd ~profiling ~debug ~unsafe ~rac ~srac ~optimize ~workers
  ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~workspace ~solver ~files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  (* deterministic_result_order implies no_stop_at_failure *)
  let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in
  let* _created_dir = Bos.OS.Dir.create ~path:true ~mode:0o755 workspace in
  let pc = Choice.return (Ok ()) in
  let result =
    List.fold_left (run_file ~unsafe ~rac ~srac ~optimize) pc files
  in
  let thread = Thread_with_memory.init () in
  let res_queue = Wq.make () in
  let path_count = ref 0 in
  let callback v =
    let open Symbolic_choice_intf in
    incr path_count;
    match (fail_mode, v) with
    | _, (EVal (Ok ()), _) -> ()
    | _, (EVal (Error e), thread) -> Wq.push (`Error e, thread) res_queue
    | (`Both | `Trap_only), (ETrap (t, m), thread) ->
      Wq.push (`ETrap (t, m), thread) res_queue
    | (`Both | `Assertion_only), (EAssert (e, m), thread) ->
      Wq.push (`EAssert (e, m), thread) res_queue
    | (`Trap_only | `Assertion_only), _ -> ()
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
  let print_bug = function
    | `ETrap (tr, model) ->
      Fmt.pr "Trap: %s@\n" (Trap.to_string tr);
      Fmt.pr "%s@\n" (Smtml.Model.to_scfg_string ~no_value model)
    | `EAssert (assertion, model) ->
      if no_assert_failure_expression_printing then begin
        Fmt.pr "Assert failure@\n"
      end
      else begin
        Fmt.pr "Assert failure: %a@\n" Expr.pp assertion
      end;
      Fmt.pr "%s@\n" (Smtml.Model.to_scfg_string ~no_value model)
  in
  let rec print_and_count_failures count_acc results =
    match results () with
    | Seq.Nil -> Ok count_acc
    | Seq.Cons ((result, _thread), tl) ->
      let* model =
        match result with
        | (`EAssert (_, model) | `ETrap (_, model)) as bug ->
          print_bug bug;
          Ok model
        | `Error e -> Error e
      in
      let count_acc = succ count_acc in
      let* () =
        if not no_value then
          let testcase = Smtml.Model.get_bindings model |> List.map snd in
          Cmd_utils.write_testcase ~dir:workspace testcase
        else Ok ()
      in
      if no_stop_at_failure then print_and_count_failures count_acc tl
      else Ok count_acc
  in
  let results =
    if deterministic_result_order then
      results
      |> Seq.map (function (_, thread) as x ->
           (x, List.rev @@ Thread_with_memory.breadcrumbs thread) )
      |> List.of_seq
      |> List.sort (fun (_, bc1) (_, bc2) ->
           List.compare Prelude.Int32.compare bc1 bc2 )
      |> List.to_seq |> Seq.map fst
    else results
  in
  let* count = print_and_count_failures 0 results in
  if print_paths then Fmt.pr "Completed paths: %d@." !path_count;
  if count > 0 then Error (`Found_bug count)
  else begin
    Fmt.pr "All OK@.";
    Ok ()
  end
