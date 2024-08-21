(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type fail_mode =
  [ `Trap_only
  | `Assertion_only
  | `Both
  ]

val cmd :
     bool
  -> bool
  -> bool
  -> bool
  -> int
  -> bool
  -> bool
  -> bool
  -> fail_mode
  -> Fpath.t
  -> Smtml.Solver_dispatcher.solver_type
  -> Fpath.t list
  -> unit Result.t
