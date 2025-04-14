(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

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
  | Double_free

val to_string : t -> string
