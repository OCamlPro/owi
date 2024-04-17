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

type 'a solver_module = (module Encoding.Solver_intf.S with type t = 'a)

type solver = S : ('a solver_module * 'a) -> solver [@@unboxed]

module Z3Batch = Encoding.Solver.Batch (Encoding.Z3_mappings)

let solver_mod : Z3Batch.t solver_module = (module Z3Batch)

let fresh_solver () =
  let module Mapping = Encoding.Z3_mappings.Fresh.Make () in
  let module Batch = Encoding.Solver.Batch (Mapping) in
  let solver = Batch.create ~logic:QF_BVFP () in
  S ((module Batch), solver)
