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

let link_symbolic_modules link_state =
  let func_typ = Symbolic.Extern_func.extern_type in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic" ~func_typ
      Symbolic_wasm_ffi.symbolic_extern_module
  in
  Link.extern_module' link_state ~name:"summaries" ~func_typ
    Symbolic_wasm_ffi.summaries_extern_module

let run_file ~entry_point ~unsafe ~rac ~srac ~optimize ~invoke_with_symbols _pc
  filename =
  let* m = Compile.File.until_binary_validate ~unsafe ~rac ~srac filename in
  let* m = Cmd_utils.set_entry_point entry_point invoke_with_symbols m in
  let link_state = link_symbolic_modules Link.empty_state in

  let+ m, link_state =
    Compile.Binary.until_link ~unsafe ~optimize ~name:None link_state m
  in
  let m = Symbolic.convert_module_to_run m in
  Interpret.Symbolic.modul link_state.envs m

let print_bug ~model_format ~model_out_file ~id ~no_value ~no_stop_at_failure
  ~no_assert_failure_expression_printing ~with_breadcrumbs bug =
  let to_string model labels breadcrumbs scoped_values =
    match model_format with
    | Cmd_utils.Json ->
      let json = Scoped_symbol.to_json model scoped_values in
      let labels_json =
        List.map
          (fun (id, name) -> `Assoc [ ("id", `Int id); ("name", `String name) ])
          labels
      in
      let json =
        match json with
        | `Assoc fields -> `Assoc (("labels", `List labels_json) :: fields)
        | _ -> json
      in
      Yojson.Basic.to_string json
    | Scfg ->
      let scfg = Scoped_symbol.to_scfg ~no_value model scoped_values in
      let model = Scfg.Query.get_dir_exn "model" scfg in
      let lbls =
        List.map
          (fun (id, lbl_name) ->
            { Scfg.Types.name = "label"
            ; params = [ string_of_int id; lbl_name ]
            ; children = []
            } )
          labels
      in
      let children =
        if with_breadcrumbs then
          let bcrumbs =
            [ { Scfg.Types.name = "breadcrumbs"
              ; params = List.map string_of_int (List.rev breadcrumbs)
              ; children = []
              }
            ]
          in
          model.children @ lbls @ bcrumbs
        else model.children @ lbls
      in
      Fmt.str "%a" Scfg.Pp.directive { model with children }
  in
  let to_file path model labels breadcrumbs scoped_symbols =
    let model_ext = match model_format with Json -> "json" | Scfg -> "scfg" in
    let contains_ext =
      Fpath.has_ext ".json" path || Fpath.has_ext ".scfg" path
    in
    let* path =
      let* () =
        if contains_ext && (not @@ Fpath.has_ext model_ext path) then
          Fmt.error_msg
            "Given model file extension is not compatible with the current \
             model format"
        else Ok ()
      in
      let path = Fpath.to_string @@ Fpath.rem_ext path in
      let base =
        Fpath.v (if no_stop_at_failure then Fmt.str "%s_%d" path id else path)
      in
      Ok (Fpath.add_ext model_ext base)
    in
    Bos.OS.File.write path (to_string model labels breadcrumbs scoped_symbols)
  in
  let output model labels breadcrumbs scoped_symbols =
    match model_out_file with
    | Some path -> to_file path model labels breadcrumbs scoped_symbols
    | None -> begin
      Logs.app (fun m ->
        m "%s" (to_string model labels breadcrumbs scoped_symbols) );
      Ok ()
    end
  in
  match bug with
  | `ETrap (tr, model, labels, breadcrumbs, scoped_symbols) ->
    Logs.err (fun m -> m "Trap: %s" (Result.err_to_string tr));
    output model labels breadcrumbs scoped_symbols
  | `EAssert (assertion, model, labels, breadcrumbs, scoped_symbols) ->
    if no_assert_failure_expression_printing then
      Logs.err (fun m -> m "Assert failure")
    else Logs.err (fun m -> m "Assert failure: %a" Expr.pp assertion);
    output model labels breadcrumbs scoped_symbols

let print_and_count_failures ~model_format ~model_out_file ~no_value
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
          let* () =
            print_bug ~model_format ~model_out_file ~id:count_acc ~no_value
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

let handle_result ~workers ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~workspace ~solver ~model_format ~model_out_file ~with_breadcrumbs
  (result : unit Symbolic.Choice.t) =
  let thread = Thread_with_memory.init () in
  let res_queue = Wq.make () in
  let path_count = Atomic.make 0 in
  let callback v =
    let open Symbolic_choice_intf in
    Atomic.incr path_count;
    match (fail_mode, v) with
    | _, (EVal (), _) -> ()
    | ( (Both | Trap_only)
      , (ETrap (t, m, labels, breadcrumbs, scoped_symbols), thread) ) ->
      Wq.push
        (`ETrap (t, m, labels, breadcrumbs, scoped_symbols), thread)
        res_queue
    | ( (Both | Assertion_only)
      , (EAssert (e, m, labels, breadcrumbs, scoped_symbols), thread) ) ->
      Wq.push
        (`EAssert (e, m, labels, breadcrumbs, scoped_symbols), thread)
        res_queue
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
    print_and_count_failures ~model_format ~model_out_file ~no_value
      ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure
      ~count_acc:0 ~results ~with_breadcrumbs
  in
  Logs.info (fun m -> m "Completed paths: %d" (Atomic.get path_count));
  let+ () = if count > 0 then Error (`Found_bug count) else Ok () in
  Logs.app (fun m -> m "All OK!")

(* NB: This function propagates potential errors (Result.err) occurring
         during evaluation (OS, syntax error, etc.), except for Trap and Assert,
         which are handled here. Most of the computations are done in the Result
         monad, hence the let*. *)
let cmd ~unsafe ~rac ~srac ~optimize ~workers ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~workspace ~solver ~files ~model_format ~entry_point ~invoke_with_symbols
  ~model_out_file ~with_breadcrumbs =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_sym_%s"
  in

  (* deterministic_result_order implies no_stop_at_failure *)
  let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in
  let pc = Choice.return () in
  let* result : unit Symbolic.Choice.t =
    list_fold_left
      (run_file ~entry_point ~unsafe ~rac ~srac ~optimize ~invoke_with_symbols)
      pc files
  in
  handle_result ~fail_mode ~workers ~solver ~deterministic_result_order
    ~model_format ~no_value ~no_assert_failure_expression_printing ~workspace
    ~no_stop_at_failure ~model_out_file ~with_breadcrumbs result
