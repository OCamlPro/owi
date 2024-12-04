(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type Runtime_events.User.tag += Solver_check

type Runtime_events.User.tag += Solver_check_true

type Runtime_events.User.tag += Solver_check_false

type Runtime_events.User.tag += Solver_model

let check =
  Runtime_events.User.register "solver_check" Solver_check
    Runtime_events.Type.span

let check_true =
  Runtime_events.User.register "solver_check_true" Solver_check_true
    Runtime_events.Type.span

let check_false =
  Runtime_events.User.register "solver_check_false" Solver_check_false
    Runtime_events.Type.span

let model =
  Runtime_events.User.register "solver_model" Solver_model
    Runtime_events.Type.span

let with_ev ev f =
  Runtime_events.User.write ev Runtime_events.Type.Begin;
  Fun.protect f ~finally:(fun () ->
    Runtime_events.User.write ev Runtime_events.Type.End )
