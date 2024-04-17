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

type t =
  { choices : int
  ; mutable symbol_set : Encoding.Symbol.t list
  ; pc : Symbolic_value.vbool list
  ; memories : Symbolic_memory.collection
  ; tables : Symbolic_table.collection
  ; globals : Symbolic_global.collection
      (** Breadcrumbs represent the list of choices that were made so far. They
          identify one given symbolic execution trace. *)
  ; breadcrumbs : int32 list
  }

let pc t = t.pc

let memories t = t.memories

let tables t = t.tables

let globals t = t.globals

let breadcrumbs t = t.breadcrumbs

let create () =
  { choices = 0
  ; symbol_set = []
  ; pc = []
  ; memories = Symbolic_memory.init ()
  ; tables = Symbolic_table.init ()
  ; globals = Symbolic_global.init ()
  ; breadcrumbs = []
  }

let clone { choices; symbol_set; pc; memories; tables; globals; breadcrumbs } =
  let memories = Symbolic_memory.clone memories in
  let tables = Symbolic_table.clone tables in
  let globals = Symbolic_global.clone globals in
  { choices; symbol_set; pc; memories; tables; globals; breadcrumbs }
