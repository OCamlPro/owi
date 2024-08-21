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
  | `Incompatible_import_type
  | `Inline_function_type
  | `Invalid_result_arity
  | `Lexer_illegal_character of string
  | `Lexer_unknown_operator of string
  | `Malformed_utf8_encoding of string
  | `Memory_size_too_large
  | `Msg of string
  | `Multiple_memories
  | `Multiple_start_sections
  | `No_error
  | `Parse_fail of string
  | `Size_minimum_greater_than_maximum
  | `Start_function
  | `Timeout
  | `Trap of Trap.t
  | `Type_mismatch of string
  | `Unbound_last_module
  | `Unbound_module of string
  | `Unbound_name of string
  | `Undeclared_function_reference
  | `Unexpected_token of string
  | `Unknown_data of Types.text Types.indice
  | `Unknown_elem of Types.text Types.indice
  | `Unknown_func of Types.text Types.indice
  | `Unknown_global of Types.text Types.indice
  | `Unknown_import of string * string
  | `Unknown_label of Types.text Types.indice
  | `Unknown_local of Types.text Types.indice
  | `Unknown_memory of Types.text Types.indice
  | `Unknown_module of string
  | `Unknown_operator
  | `Unknown_table of Types.text Types.indice
  | `Unknown_type of Types.text Types.indice
  | `Unsupported_file_extension of string
  | `Spec_invalid_int32 of string
  | `Spec_invalid_int64 of string
  | `Spec_invalid_float32 of string
  | `Spec_invalid_float64 of string
  | `Spec_invalid_indice of string
  | `Spec_invalid_text_indice of string
  | `Unknown_annotation_clause of Sexp.t
  | `Unknown_annotation_object of Sexp.t
  | `Spec_unknown_binder of Types.text Types.indice
  | `Spec_unknown_param of Types.text Types.indice
  | `Spec_unknown_variable of Types.text Types.indice
  | `Spec_unknown_binder_type of Sexp.t
  | `Spec_unknown_prop of Sexp.t
  | `Spec_unknown_term of Sexp.t
  | `Spec_type_error of string
  | `Contract_unknown_func of Types.text Types.indice
  | `Empty_annotation_id
  | `Empty_identifier
  | `Unclosed_annotation
  | `Unclosed_comment
  | `Unclosed_string
  ]

type 'a t = ('a, err) Prelude.Result.t

val err_to_string : err -> string
