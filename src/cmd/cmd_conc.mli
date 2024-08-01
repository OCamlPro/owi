(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     bool
  -> bool
  -> bool
  -> bool
  -> int
  -> bool
  -> bool
  -> bool
  -> Cmd_sym.fail_mode
  -> Fpath.t
  -> Smtml.Solver_dispatcher.solver_type
  -> Fpath.t list
  -> unit Result.t
