(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file ~entry_point ~symbolic_parameters ~source_file =
  let { Symbolic_parameters.unsafe
      ; invoke_with_symbols
      ; exploration_strategy = _
      ; generate_abstract_invariant
      ; fail_mode
      ; timeout
      ; timeout_instr
      ; use_ite_for_select
      ; _
      } =
    symbolic_parameters
  in
  let* modul = Compile.Wasm.File.until_validate ~unsafe source_file in
  (* TODO: enable this once the smart strategy is fully implemented
  ( match exploration_strategy with
  | Smart -> Cmd_call_graph.compute_distances m entry_point
  | _ -> () );
  *)
  let* modul =
    Cmd_utils.set_entry_point entry_point invoke_with_symbols modul
  in

  let* abstract_invariant =
    if generate_abstract_invariant then
      let* env = Cmd_wasm_abs.env () in
      let+ modul, env =
        Compile.Wasm.Binary.until_abstract_link ~unsafe ~name:None env modul
      in
      try
        let state = Abstract_interpreter_control_flow.modul ~env ~modul in
        state.invariant
      with Abstract_interpreter_control_flow.RecursiveFunctionCall ->
        Abstract_invariant.empty ()
    else Ok (Abstract_invariant.empty ())
  in

  let env = Env.Symbolic.empty ~context:() in
  let* env =
    Env.Symbolic.link_extern_module ~env ~name:"wasi_snapshot_preview1"
      Symbolic_wasm_ffi.wasi_snapshot_preview1
  in
  let* env =
    Env.Symbolic.link_extern_module ~env ~name:"owi" Symbolic_wasm_ffi.owi
  in
  let+ modul, env =
    (* unsafe is set to true because the module was already validated before *)
    Compile.Wasm.Binary.until_symbolic_link env ~unsafe:true ~name:None modul
  in
  let module Parameters = struct
    let throw_away_trap =
      match fail_mode with Assertion_only -> true | Both | Trap_only -> false

    let timeout = timeout

    let timeout_instr = timeout_instr

    let use_ite_for_select = use_ite_for_select

    let abstract_invariant = abstract_invariant
  end in
  let module I = Interpret.Symbolic (Parameters) in
  Benchmark.with_utime @@ fun () -> I.modul ~env ~modul

(* NB: This function propagates potential errors (Result.err) occurring
             during evaluation (OS, syntax error, etc.), except for Trap and Assert,
             which are handled here. Most of the computations are done in the Result
             monad, hence the let*. *)
let cmd ~entry_point ~source_file ~(symbolic_parameters : Symbolic_parameters.t)
    =
  (* deterministic_result_order implies no_stop_at_failure *)
  let no_stop_at_failure =
    (* TODO: move this somewhere else *)
    symbolic_parameters.deterministic_result_order
    || symbolic_parameters.no_stop_at_failure
  in

  (* TODO: can we handle this at the cmdliner level? *)
  let* workspace =
    match symbolic_parameters.workspace with
    | Some path -> Ok path
    | None -> Bos.OS.Dir.tmp "owi_sym_%s"
  in

  let* to_run, run_time =
    run_file ~symbolic_parameters ~source_file ~entry_point
  in

  let { Symbolic_parameters.exploration_strategy
      ; fail_mode
      ; workers
      ; no_worker_isolation
      ; solver
      ; deterministic_result_order
      ; model_format
      ; no_value
      ; no_assert_failure_expression_printing
      ; model_out_file
      ; with_breadcrumbs
      ; seed
      ; _
      } =
    symbolic_parameters
  in

  Symbolic_driver.run ~exploration_strategy ~fail_mode ~workers
    ~no_worker_isolation ~solver ~deterministic_result_order ~model_format
    ~no_value ~no_assert_failure_expression_printing ~workspace
    ~no_stop_at_failure ~model_out_file ~with_breadcrumbs ~seed ~run_time to_run
