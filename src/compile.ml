(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*  Written by Léo Andrès and Pierre Chambart                                *)
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

let ( let* ) o f = match o with Error msg -> Error msg | Ok v -> f v

let until_check ~unsafe m = if unsafe then Ok m else Check.modul m

let until_group ~unsafe m =
  let* m = until_check ~unsafe m in
  let* m = Grouped.of_symbolic m in
  Ok m

let until_assign ~unsafe m =
  let* m = until_group ~unsafe m in
  let* m = Assigned.of_grouped m in
  Ok m

let until_simplify ~unsafe m =
  let* m = until_assign ~unsafe m in
  let* m = Rewrite.modul m in
  Ok m

let until_typecheck ~unsafe m =
  let* m = until_simplify ~unsafe m in
  if unsafe then Ok m
  else
    let* () = Typecheck.modul m in
    Ok m

let until_optimize ~unsafe ~optimize m =
  let* m = until_typecheck ~unsafe m in
  if optimize then Ok (Optimize.modul m) else Ok m

let until_link ~unsafe link_state ~optimize ~name m =
  let* m = until_optimize ~unsafe ~optimize m in
  Link.modul link_state ~name m

let until_interpret link_state ~unsafe ~optimize ~name m =
  let* m, link_state = until_link link_state ~unsafe ~optimize ~name m in
  let* () = Interpret.Concrete.modul link_state.envs m in
  Ok link_state
