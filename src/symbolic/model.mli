(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* model stuff *)
type output_format =
  | Scfg
  | Json

val print :
     format:output_format
  -> out_file:Fpath.t option
  -> id:int
  -> no_value:bool
  -> no_stop_at_failure:bool
  -> no_assert_failure_expression_printing:bool
  -> with_breadcrumbs:bool
  -> [ `Trap of Bug.trap | `Assertion of Bug.assertion ]
  -> unit Result.t
