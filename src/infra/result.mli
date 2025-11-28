(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include module type of Prelude.Result

type err =
  [ `Alignment_too_large
  | `Assert_failure
  | `Bad_result
  | `Call_stack_exhausted
  | `Constant_expression_required
  | `Constant_out_of_range
  | `Did_not_fail_but_expected of string
  | `Duplicate_export_name
  | `Duplicate_global of string
  | `Duplicate_local of string
  | `Duplicate_memory of string
  | `Duplicate_table of string
  | `Failed_with_but_expected of err * string
  | `Found_bug of int
  | `Global_is_immutable
  | `Illegal_escape of string
  | `Import_after_function
  | `Import_after_global
  | `Import_after_memory
  | `Import_after_table
  | `Incompatible_import_type of string
  | `Inline_function_type
  | `Invalid_character_in_memory
  | `Invalid_result_arity
  | `Lexer_illegal_character of string
  | `Lexer_unknown_operator of string
  | `Malformed_utf8_encoding of string
  | `Memory_size_too_large
  | `Msg of string
  | `Multiple_start_sections
  | `No_error
  | `Parse_fail of string
  | `Size_minimum_greater_than_maximum
  | `Start_function
  | `Timeout
  | `Type_mismatch of string
  | `Unbound_last_module
  | `Unbound_module of string
  | `Unbound_name of string
  | `Undeclared_function_reference
  | `Unexpected_token of string
  | `Unknown_data of Text.indice
  | `Unknown_elem of Text.indice
  | `Unknown_func of Text.indice
  | `Unknown_global of Text.indice
  | `Unknown_import of string * string
  | `Unknown_label of Text.indice
  | `Unknown_local of Text.indice
  | `Unknown_memory of Text.indice
  | `Unknown_export of Text.indice
  | `Unknown_module of string
  | `Unknown_operator
  | `Unknown_table of Text.indice
  | `Unknown_type of Text.indice
  | `Unsupported_file_extension of string
  | `Contract_unknown_func of Text.indice
  | `Empty_annotation_id
  | `Empty_identifier
  | `Unclosed_annotation
  | `Unclosed_comment
  | `Unclosed_string
  | `Unbounded_quantification
  | `Invalid_model of string
  | `Unimplemented of string
  | (* Trap: *)
    `Out_of_bounds_table_access
  | `Out_of_bounds_memory_access
  | `Undefined_element
  | `Uninitialized_element of int
  | `Integer_overflow
  | `Integer_divide_by_zero
  | `Conversion_to_integer
  | `Element_type_error
  | `Unreachable
  | `Indirect_call_type_mismatch
  | `Extern_call_arg_type_mismatch
  | `Extern_call_null_arg
  | `Memory_leak_use_after_free
  | `Memory_heap_buffer_overflow
  | `Double_free
  ]

type 'a t = ('a, err) Prelude.Result.t

val err_to_string : err -> string

val err_to_exit_code : err -> int
