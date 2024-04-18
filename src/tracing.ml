(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

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
