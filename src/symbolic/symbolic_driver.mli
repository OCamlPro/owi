(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val handle_result :
     exploration_strategy:Symbolic_parameters.Exploration_strategy.t
  -> workers:int
  -> no_stop_at_failure:bool
  -> no_value:bool
  -> no_assert_failure_expression_printing:bool
  -> deterministic_result_order:bool
  -> fail_mode:Symbolic_parameters.fail_mode
  -> workspace:Fpath.t
  -> solver:Smtml.Solver_type.t
  -> model_format:Model.output_format
  -> model_out_file:Fpath.t option
  -> with_breadcrumbs:bool
  -> run_time:float option
  -> unit Symbolic_choice.t
  -> unit Result.t
