(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     debug:bool
  -> arch:int
  -> property:Fpath.t option
  -> testcomp:bool
  -> workspace:Fpath.t
  -> workers:int
  -> opt_lvl:string
  -> includes:Fpath.t list
  -> files:Fpath.t list
  -> profiling:bool
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
  -> profile:Fpath.t option
  -> unit Result.t
