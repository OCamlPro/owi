(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Stdlib.Result

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
  | `Unexpected_token
  | `Unknown_function of int
  | `Unknown_global
  | `Unknown_import
  | `Unknown_label
  | `Unknown_local of string
  | `Unknown_memory of int
  | `Unknown_module of string
  | `Unknown_operator
  | `Unknown_type
  | `Unsupported_file_extension of string
  ]

type 'a t = ('a, err) Stdlib.Result.t

let rec err_to_string = function
  | `Alignment_too_large -> "alignment must not be larger than natural"
  | `Assert_failure -> "script assert failure"
  | `Bad_result -> "bad result"
  | `Call_stack_exhausted -> "call stack exhausted"
  | `Constant_expression_required -> "constant expression required"
  | `Constant_out_of_range -> "constant out of range"
  | `Did_not_fail_but_expected expected ->
    Format.sprintf "expected %s but there was no error" expected
  | `Duplicate_export_name -> "duplicate export name"
  | `Duplicate_global id -> Format.sprintf "duplicate global %s" id
  | `Duplicate_local id -> Format.sprintf "duplicate local %s" id
  | `Duplicate_memory id -> Format.sprintf "duplicate memory %s" id
  | `Duplicate_table id -> Format.sprintf "duplicate table %s" id
  | `Failed_with_but_expected (got, expected) ->
    Format.sprintf "expected %s but got (%s)" expected (err_to_string got)
  | `Found_bug n ->
    if n > 1 then Format.sprintf "Reached %d problems!" n
    else Format.sprintf "Reached problem!"
  | `Global_is_immutable -> "global is immutable"
  | `Illegal_escape txt -> Format.sprintf "illegal escape %S" txt
  | `Import_after_function -> "import after function"
  | `Import_after_global -> "import after global"
  | `Import_after_memory -> "import after memory"
  | `Import_after_table -> "import after table"
  | `Incompatible_import_type -> "incompatible import type"
  | `Inline_function_type -> "inline function type"
  | `Invalid_result_arity -> "invalid result arity"
  | `Lexer_unknown_operator op -> Format.sprintf "unknown operator %s" op
  | `Malformed_utf8_encoding txt ->
    Format.sprintf "malformed UTF-8 encoding %S" txt
  | `Memory_size_too_large -> "memory size must be at most 65536 pages (4GiB)"
  | `Msg msg -> msg
  | `Multiple_memories -> "multiple memories"
  | `Multiple_start_sections -> "multiple start sections"
  | `No_error -> "no error"
  | `Parse_fail msg -> msg
  | `Size_minimum_greater_than_maximum ->
    "size minimum must not be greater than maximum"
  | `Start_function -> "start function must have type [] -> []"
  | `Timeout -> "timeout"
  | `Trap t -> Format.sprintf "trap: %s" (Trap.to_string t)
  | `Type_mismatch msg -> Format.sprintf "type mismatch (%s)" msg
  | `Unbound_last_module -> "unbound last module"
  | `Unbound_module id -> Format.sprintf "unbound module %s" id
  | `Unbound_name id -> Format.sprintf "unbound name %s" id
  | `Undeclared_function_reference -> "undeclared function reference"
  | `Unexpected_token -> "unexpected token"
  | `Unknown_function id -> Format.sprintf "unknown function %d" id
  | `Unknown_global -> "unknown global"
  | `Unknown_import -> "unknown import"
  | `Unknown_label -> "unknown label"
  | `Unknown_local id -> Format.sprintf "unknown local %s" id
  | `Unknown_memory id -> Format.sprintf "unknown memory %d" id
  | `Unknown_module id -> Format.sprintf "unknown module %s" id
  | `Unknown_operator -> Format.sprintf "unknown operator"
  | `Unknown_type -> "unknown type"
  | `Unsupported_file_extension ext ->
    Format.sprintf "unsupported file_extension %S" ext

let failwith e = failwith (err_to_string e)
