(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     unsafe:bool
  -> rac:bool
  -> srac:bool
  -> optimize:bool
  -> workers:int
  -> no_stop_at_failure:bool
  -> no_value:bool
  -> no_assert_failure_expression_printing:bool
  -> deterministic_result_order:bool
  -> fail_mode:Cmd_sym.fail_mode
  -> workspace:Fpath.t option
  -> solver:Smtml.Solver_type.t
  -> files:Fpath.t list
  -> model_format:Cmd_utils.model_format
  -> entry_point:string option
  -> invoke_with_symbols:bool
  -> model_out_file:Fpath.t option
  -> with_breadcrumbs:bool
  -> model_with_entry_point:bool
  -> unit Result.t
