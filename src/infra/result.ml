(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Prelude.Result

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
  | `Out_of_bounds_table_access
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

let rec err_to_string = function
  | `Alignment_too_large -> "alignment must not be larger than natural"
  | `Assert_failure -> "script assert failure"
  | `Bad_result -> "bad result"
  | `Call_stack_exhausted -> "call stack exhausted"
  | `Constant_expression_required -> "constant expression required"
  | `Constant_out_of_range -> "constant out of range"
  | `Did_not_fail_but_expected expected ->
    Fmt.str "expected %s but there was no error" expected
  | `Duplicate_export_name -> "duplicate export name"
  | `Duplicate_global id -> Fmt.str "duplicate global %s" id
  | `Duplicate_local id -> Fmt.str "duplicate local %s" id
  | `Duplicate_memory id -> Fmt.str "duplicate memory %s" id
  | `Duplicate_table id -> Fmt.str "duplicate table %s" id
  | `Failed_with_but_expected (got, expected) ->
    Fmt.str "expected %s but got (%s)" expected (err_to_string got)
  | `Found_bug n ->
    if n > 1 then Fmt.str "Reached %d problems!" n
    else Fmt.str "Reached problem!"
  | `Global_is_immutable -> "global is immutable"
  | `Illegal_escape txt -> Fmt.str "illegal escape %S" txt
  | `Import_after_function -> "import after function"
  | `Import_after_global -> "import after global"
  | `Import_after_memory -> "import after memory"
  | `Import_after_table -> "import after table"
  | `Incompatible_import_type name ->
    Fmt.str "incompatible import type for %s" name
  | `Inline_function_type -> "inline function type"
  | `Invalid_character_in_memory -> "Invalide character in_memory"
  | `Invalid_result_arity -> "invalid result arity"
  | `Lexer_illegal_character c -> Fmt.str "%s" c
  | `Lexer_unknown_operator op -> Fmt.str "%s" op
  | `Malformed_utf8_encoding txt -> Fmt.str "malformed UTF-8 encoding %S" txt
  | `Memory_size_too_large -> "memory size must be at most 65536 pages (4GiB)"
  | `Msg msg -> msg
  | `Multiple_start_sections -> "multiple start sections"
  | `No_error -> "no error"
  | `Parse_fail msg -> msg
  | `Size_minimum_greater_than_maximum ->
    "size minimum must not be greater than maximum"
  | `Start_function -> "start function must have type [] -> []"
  | `Timeout -> "timeout"
  | `Type_mismatch msg -> Fmt.str "type mismatch (%s)" msg
  | `Unbound_last_module -> "unbound last module"
  | `Unbound_module id -> Fmt.str "unbound module %s" id
  | `Unbound_name id -> Fmt.str "unbound name %s" id
  | `Undeclared_function_reference -> "undeclared function reference"
  | `Unexpected_token s -> Fmt.str "unexpected token %S" s
  | `Unknown_data id -> Fmt.str "unknown data segment %a" Text.pp_indice id
  | `Unknown_elem id -> Fmt.str "unknown elem segment %a" Text.pp_indice id
  | `Unknown_func id -> Fmt.str "unknown function %a" Text.pp_indice id
  | `Unknown_global id -> Fmt.str "unknown global %a" Text.pp_indice id
  | `Unknown_import (modul, value) -> Fmt.str "unknown import %S %S" modul value
  | `Unknown_label id -> Fmt.str "unknown label %a" Text.pp_indice id
  | `Unknown_local id -> Fmt.str "unknown local %a" Text.pp_indice id
  | `Unknown_memory id -> Fmt.str "unknown memory %a" Text.pp_indice id
  | `Unknown_export id -> Fmt.str "unknown export %a" Text.pp_indice id
  | `Unknown_module name -> Fmt.str "unknown module %s" name
  | `Unknown_operator -> Fmt.str "unknown operator"
  | `Unknown_table id -> Fmt.str "unknown table %a" Text.pp_indice id
  | `Unknown_type id -> Fmt.str "unknown type %a" Text.pp_indice id
  | `Unsupported_file_extension ext ->
    Fmt.str "unsupported file_extension %S" ext
  | `Contract_unknown_func id ->
    Fmt.str "contract: unknown function %a" Text.pp_indice id
  | `Empty_annotation_id -> Fmt.str "empty annotation id"
  | `Empty_identifier -> Fmt.str "empty identifier"
  | `Unclosed_annotation -> Fmt.str "unclosed annotation"
  | `Unclosed_comment -> Fmt.str "unclosed comment"
  | `Unclosed_string -> Fmt.str "unclosed string"
  | `Unbounded_quantification -> Fmt.str "unbounded quantification"
  | `Invalid_model msg -> Fmt.str "invalid model: %s" msg
  | `Unimplemented msg -> Fmt.str "unimplemented: %s" msg
  | `Out_of_bounds_table_access -> "out of bounds table access"
  | `Out_of_bounds_memory_access -> "out of bounds memory access"
  | `Undefined_element -> "undefined element"
  | `Uninitialized_element fun_i -> Fmt.str "uninitialized element %i" fun_i
  | `Integer_overflow -> "integer overflow"
  | `Integer_divide_by_zero -> "integer divide by zero"
  | `Conversion_to_integer -> "invalid conversion to integer"
  | `Element_type_error -> "element_type_error"
  | `Unreachable -> "unreachable"
  | `Indirect_call_type_mismatch -> "indirect call type mismatch"
  | `Extern_call_arg_type_mismatch -> "extern call arg type mismatch"
  | `Extern_call_null_arg -> "extern call null arg"
  | `Memory_leak_use_after_free -> "memory leak use after free"
  | `Memory_heap_buffer_overflow -> "memory heap buffer overflow"
  | `Double_free -> "double free"

let err_to_exit_code = function
  | `No_error -> Cmdliner.Cmd.Exit.ok
  | `Alignment_too_large -> 1
  | `Assert_failure -> 2
  | `Bad_result -> 3
  | `Call_stack_exhausted -> 4
  | `Constant_expression_required -> 5
  | `Constant_out_of_range -> 6
  | `Did_not_fail_but_expected _ -> 7
  | `Duplicate_export_name -> 8
  | `Duplicate_global _id -> 9
  | `Duplicate_local _id -> 10
  | `Duplicate_memory _id -> 11
  | `Duplicate_table _id -> 12
  | `Found_bug _count -> 13
  | `Global_is_immutable -> 14
  | `Illegal_escape _txt -> 15
  | `Import_after_function -> 16
  | `Import_after_global -> 17
  | `Import_after_memory -> 18
  | `Import_after_table -> 19
  | `Incompatible_import_type _name -> 20
  | `Inline_function_type -> 21
  | `Invalid_result_arity -> 22
  | `Lexer_illegal_character _c -> 23
  | `Lexer_unknown_operator _op -> 23
  | `Malformed_utf8_encoding _txt -> 24
  | `Memory_size_too_large -> 25
  | `Msg _msg -> 26
  | `Multiple_start_sections -> 28
  | `Parse_fail _txt -> 30
  | `Size_minimum_greater_than_maximum -> 31
  | `Start_function -> 32
  | `Timeout -> 33
  | `Double_free -> 34
  | `Type_mismatch _msg -> 35
  | `Unbound_last_module -> 36
  | `Unbound_module _id -> 37
  | `Unbound_name _id -> 38
  | `Undeclared_function_reference -> 39
  | `Unexpected_token _token -> 40
  | `Unknown_data _id -> 41
  | `Unknown_elem _id -> 42
  | `Unknown_func _id -> 43
  | `Unknown_global _id -> 44
  | `Unknown_import _ -> 45
  | `Unknown_label _id -> 46
  | `Unknown_local _id -> 47
  | `Unknown_memory _id -> 48
  | `Unknown_module _id -> 49
  | `Unknown_operator -> 50
  | `Unknown_table _id -> 51
  | `Unknown_type _id -> 52
  | `Unsupported_file_extension _ext -> 53
  | `Failed_with_but_expected (_got, _expected) -> 54
  | `Spec_invalid_int32 _i32 -> 56
  | `Spec_invalid_int64 _i64 -> 57
  | `Spec_invalid_float32 _f32 -> 58
  | `Spec_invalid_float64 _f64 -> 59
  | `Spec_invalid_indice _id -> 60
  | `Spec_invalid_text_indice _id -> 61
  | `Unknown_annotation_clause _s -> 62
  | `Unknown_annotation_object _s -> 63
  | `Spec_unknown_binder _id -> 64
  | `Spec_unknown_param _id -> 65
  | `Spec_unknown_variable _id -> 66
  | `Spec_unknown_binder_type _s -> 67
  | `Spec_unknown_prop _pr -> 68
  | `Spec_unknown_term _tm -> 69
  | `Spec_type_error _str -> 70
  | `Contract_unknown_func _id -> 71
  | `Empty_annotation_id -> 72
  | `Empty_identifier -> 73
  | `Unclosed_annotation -> 74
  | `Unclosed_comment -> 75
  | `Unclosed_string -> 76
  | `Unbounded_quantification -> 77
  | `Invalid_model _msg -> 78
  | `Unknown_export _id -> 79
  | `Unimplemented _msg -> 80
  | `Element_type_error -> 81
  | `Extern_call_arg_type_mismatch -> 82
  | `Extern_call_null_arg -> 83
  | `Indirect_call_type_mismatch -> 84
  | `Integer_divide_by_zero -> 85
  | `Integer_overflow -> 86
  | `Conversion_to_integer -> 87
  | `Memory_heap_buffer_overflow -> 88
  | `Memory_leak_use_after_free -> 89
  | `Out_of_bounds_memory_access -> 90
  | `Out_of_bounds_table_access -> 91
  | `Undefined_element -> 92
  | `Uninitialized_element _ -> 93
  | `Unreachable -> 94
  | `Invalid_character_in_memory -> 95
