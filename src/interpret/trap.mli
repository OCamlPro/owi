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
  | Out_of_bounds_table_access
  | Out_of_bounds_memory_access
  | Undefined_element
  | Uninitialized_element of int
  | Integer_overflow
  | Integer_divide_by_zero
  | Element_type_error
  | Unreachable
  | Indirect_call_type_mismatch
  | Extern_call_arg_type_mismatch
  | Extern_call_null_arg
  | Memory_leak_use_after_free
  | Memory_heap_buffer_overflow

val to_string : t -> string
