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

let to_string = function
  | Out_of_bounds_table_access -> "out of bounds table access"
  | Out_of_bounds_memory_access -> "out of bounds memory access"
  | Undefined_element -> "undefined element"
  | Uninitialized_element fun_i ->
    Printf.sprintf "uninitialized element %i" fun_i
  | Integer_overflow -> "integer overflow"
  | Integer_divide_by_zero -> "integer divide by zero"
  | Element_type_error -> "element_type_error"
  | Unreachable -> "unreachable"
  | Indirect_call_type_mismatch -> "indirect call type mismatch"
  | Extern_call_arg_type_mismatch -> "extern call arg type mismatch"
  | Extern_call_null_arg -> "extern call null arg"
  | Memory_leak_use_after_free -> "memory leak use after free"
  | Memory_heap_buffer_overflow -> "memory heap buffer overflow"
