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

open Types

type tbl = (string, int) Hashtbl.t Option.t

val equal_func_types : simplified func_type -> simplified func_type -> bool

val convert_val_type : tbl -> text val_type -> simplified val_type Result.t

val convert_heap_type : tbl -> text heap_type -> simplified heap_type Result.t

val convert_func_type : tbl -> text func_type -> simplified func_type Result.t

val convert_ref_type : tbl -> text ref_type -> simplified ref_type Result.t

val convert_param : tbl -> text param -> simplified param Result.t

val convert_table_type :
  tbl -> text table_type -> simplified table_type Result.t

val convert_str : tbl -> text str_type -> simplified str_type Result.t
