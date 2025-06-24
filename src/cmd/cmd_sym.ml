(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax
module Expr = Smtml.Expr

type fail_mode =
  | Trap_only
  | Assertion_only
  | Both

type exploration_strategy =
  | FIFO
  | LIFO
  | Random

type parameters =
  { unsafe : bool
  ; rac : bool
  ; srac : bool
  ; optimize : bool
  ; workers : int
  ; no_stop_at_failure : bool
  ; no_value : bool
  ; no_assert_failure_expression_printing : bool
  ; deterministic_result_order : bool
  ; fail_mode : fail_mode
  ; exploration_strategy : exploration_strategy
  ; workspace : Fpath.t option
  ; solver : Smtml.Solver_type.t
  ; model_format : Cmd_utils.model_format
  ; entry_point : string option
  ; invoke_with_symbols : bool
  ; model_out_file : Fpath.t option
  ; with_breadcrumbs : bool
  }

let run_file ~parameters ~source_file =
  let { unsafe; rac; srac; entry_point; invoke_with_symbols; optimize; _ } =
    parameters
  in
  let* m = Compile.File.until_validate ~unsafe ~rac ~srac source_file in
  let* m = Cmd_utils.set_entry_point entry_point invoke_with_symbols m in
  let link_state =
    let func_typ = Symbolic.Extern_func.extern_type in
    let link_state = Link.empty_state in
    let link_state =
      Link.extern_module' link_state ~name:"wasi_snapshot_preview1" ~func_typ
        Symbolic_wasm_ffi.wasi_snapshot_preview1
    in
    let link_state =
      Link.extern_module' link_state ~name:"owi" ~func_typ
        Symbolic_wasm_ffi.symbolic_extern_module
    in
    link_state
  in

  let+ m, link_state =
    Compile.Binary.until_link ~unsafe ~optimize ~name:None link_state m
  in
  let m = Symbolic.convert_module_to_run m in
  Interpret.Symbolic.modul ~timeout:None ~timeout_instr:None link_state.envs m

let print_bug ~model_format ~model_out_file ~id ~no_value ~no_stop_at_failure
  ~no_assert_failure_expression_printing ~with_breadcrumbs bug =
  let pp fmt (model, labels, breadcrumbs, scoped_values) =
    match model_format with
    | Cmd_utils.Json ->
      let json = Symbol_scope.to_json ~no_value model scoped_values in
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
      let json =
        if with_breadcrumbs then
          let crumbs =
            List.rev_map (fun crumb -> `Int crumb) (List.rev breadcrumbs)
          in
          match json with
          | `Assoc fields -> `Assoc (fields @ [ ("breadcrumbs", `List crumbs) ])
          | _ -> json
        else json
      in
      Yojson.Basic.pretty_print fmt json
    | Scfg ->
      let scfg = Symbol_scope.to_scfg ~no_value model scoped_values in
      let model = Scfg.Query.get_dir_exn "model" scfg in
      let lbls =
        { Scfg.Types.name = "labels"
        ; params = []
        ; children =
            List.map
              (fun (id, lbl_name) ->
                { Scfg.Types.name = "label"
                ; params = [ string_of_int id; lbl_name ]
                ; children = []
                } )
              labels
        }
      in
      let bcrumbs =
        if with_breadcrumbs then
          [ { Scfg.Types.name = "breadcrumbs"
            ; params = List.map string_of_int (List.rev breadcrumbs)
            ; children = []
            }
          ]
        else []
      in
      let ret =
        model
        :: (if List.length lbls.children > 0 then lbls :: bcrumbs else bcrumbs)
      in
      Scfg.Pp.config fmt ret
  in
  let to_file path model labels breadcrumbs symbol_scopes =
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
    Bos.OS.File.writef path "%a" pp (model, labels, breadcrumbs, symbol_scopes)
  in
  let output model labels breadcrumbs symbol_scopes =
    match model_out_file with
    | Some path -> to_file path model labels breadcrumbs symbol_scopes
    | None -> begin
      Logs.app (fun m ->
        let fmt = m (if no_stop_at_failure then "%a@." else "%a") in
        fmt pp (model, labels, breadcrumbs, symbol_scopes) );
      Ok ()
    end
  in
  match bug with
  | `ETrap (tr, model, labels, breadcrumbs, symbol_scopes) ->
    Logs.err (fun m -> m "Trap: %s" (Result.err_to_string tr));
    output model labels breadcrumbs symbol_scopes
  | `EAssert (assertion, model, labels, breadcrumbs, symbol_scopes) ->
    if no_assert_failure_expression_printing then
      Logs.err (fun m -> m "Assert failure")
    else Logs.err (fun m -> m "Assert failure: %a" Expr.pp assertion);
    output model labels breadcrumbs symbol_scopes

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

let handle_result ~exploration_strategy ~workers ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~workspace ~solver ~model_format ~model_out_file ~with_breadcrumbs
  (result : unit Symbolic.Choice.t) =
  let thread = Thread_with_memory.init () in
  let res_stack = Ws.make () in
  let path_count = Atomic.make 0 in
  let callback v =
    let open Symbolic_choice_intf in
    Atomic.incr path_count;
    match (fail_mode, v) with
    | _, (EVal (), _) -> ()
    | ( (Both | Trap_only)
      , (ETrap (t, m, labels, breadcrumbs, symbol_scopes), thread) ) ->
      Ws.push
        (`ETrap (t, m, labels, breadcrumbs, symbol_scopes), thread)
        0 res_stack
    | ( (Both | Assertion_only)
      , (EAssert (e, m, labels, breadcrumbs, symbol_scopes), thread) ) ->
      Ws.push
        (`EAssert (e, m, labels, breadcrumbs, symbol_scopes), thread)
        0 res_stack
    | (Trap_only | Assertion_only), _ -> ()
  in
  let join_handles =
    Symbolic_choice_with_memory.run
      ( match exploration_strategy with
      | LIFO -> (module Wq)
      | FIFO -> (module Ws)
      | Random -> (module Wpq) )
      ~workers solver result thread ~callback
      ~callback_init:(fun () -> Ws.make_pledge res_stack)
      ~callback_end:(fun () -> Ws.end_pledge res_stack)
  in
  let results =
    Ws.read_as_seq res_stack ~finalizer:(fun () ->
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
let cmd ~parameters ~source_file =
  let* result : unit Symbolic.Choice.t = run_file ~parameters ~source_file in

  let { exploration_strategy
      ; fail_mode
      ; workers
      ; solver
      ; deterministic_result_order
      ; model_format
      ; no_value
      ; no_assert_failure_expression_printing
      ; workspace
      ; model_out_file
      ; with_breadcrumbs
      ; _
      } =
    parameters
  in

  (* deterministic_result_order implies no_stop_at_failure *)
  let no_stop_at_failure =
    parameters.deterministic_result_order || parameters.no_stop_at_failure
  in

  (* TODO: can we handle this at the cmdliner level? *)
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_sym_%s"
  in

  handle_result ~exploration_strategy ~fail_mode ~workers ~solver
    ~deterministic_result_order ~model_format ~no_value
    ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure
    ~model_out_file ~with_breadcrumbs result
