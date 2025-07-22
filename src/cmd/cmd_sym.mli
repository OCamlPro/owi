(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

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
  ; timeout : float option
  ; timeout_instr : int option
  }

val handle_result :
     exploration_strategy:exploration_strategy
  -> workers:int
  -> no_stop_at_failure:bool
  -> no_value:bool
  -> no_assert_failure_expression_printing:bool
  -> deterministic_result_order:bool
  -> fail_mode:fail_mode
  -> workspace:Fpath.t
  -> solver:Smtml.Solver_type.t
  -> model_format:Cmd_utils.model_format
  -> model_out_file:Fpath.t option
  -> with_breadcrumbs:bool
  -> unit Symbolic.Choice.t
  -> unit Result.t

val cmd : parameters:parameters -> source_file:Fpath.t -> unit Result.t
