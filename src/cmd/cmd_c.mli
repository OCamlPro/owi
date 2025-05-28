(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     arch:int
  -> property:Fpath.t option
  -> testcomp:bool
  -> workspace:Fpath.t option
  -> workers:int
  -> opt_lvl:string
  -> includes:Fpath.t list
  -> files:Fpath.t list
  -> unsafe:bool
  -> optimize:bool
  -> no_stop_at_failure:bool
  -> no_value:bool
  -> no_assert_failure_expression_printing:bool
  -> deterministic_result_order:bool
  -> fail_mode:Cmd_sym.fail_mode
  -> concolic:bool
  -> eacsl:bool
  -> solver:Smtml.Solver_type.t
  -> model_format:Cmd_utils.model_format
  -> entry_point:string option
  -> invoke_with_symbols:bool
  -> out_file:Fpath.t option
  -> model_out_file:Fpath.t option
  -> with_breadcrumbs:bool
  -> model_with_entry_point:bool
  -> unit Result.t
