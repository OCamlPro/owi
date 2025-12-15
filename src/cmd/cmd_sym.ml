(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

let run_file ~parameters ~source_file =
  let { Symbolic_parameters.unsafe
      ; entry_point
      ; invoke_with_symbols
      ; exploration_strategy = _
      ; _
      } =
    parameters
  in
  let* m = Compile.File.until_validate ~unsafe source_file in
  (* TODO: enable this once the smart strategy is fully implemented
  ( match exploration_strategy with
  | Smart -> Cmd_call_graph.compute_distances m entry_point
  | _ -> () );
  *)
  let* m = Cmd_utils.set_entry_point entry_point invoke_with_symbols m in
  let link_state =
    Link.State.empty ()
    |> Link.Extern.modul ~name:"wasi_snapshot_preview1"
         Symbolic_wasm_ffi.wasi_snapshot_preview1
    |> Link.Extern.modul ~name:"owi" Symbolic_wasm_ffi.symbolic_extern_module
  in
  let+ m, link_state =
    (* unsafe is set to true because the module was already validated before *)
    Compile.Binary.until_link ~unsafe:true ~name:None link_state m
  in
  let module Parameters = struct
    let throw_away_trap =
      match parameters.fail_mode with
      | Assertion_only -> true
      | Both | Trap_only -> false

    let timeout = None

    let timeout_instr = None

    let use_ite_for_select = parameters.use_ite_for_select
  end in
  let module I = Interpret.Symbolic (Parameters) in
  Benchmark.with_utime @@ fun () -> I.modul link_state m

(* NB: This function propagates potential errors (Result.err) occurring
             during evaluation (OS, syntax error, etc.), except for Trap and Assert,
             which are handled here. Most of the computations are done in the Result
             monad, hence the let*. *)
let cmd ~parameters ~source_file =
  let { Symbolic_parameters.exploration_strategy
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

  let* result, run_time = run_file ~parameters ~source_file in

  Symbolic_driver.handle_result ~exploration_strategy ~fail_mode ~workers
    ~solver ~deterministic_result_order ~model_format ~no_value
    ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure
    ~model_out_file ~with_breadcrumbs ~run_time result
