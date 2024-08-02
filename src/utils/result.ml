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
  | `Incompatible_import_type -> "incompatible import type"
  | `Inline_function_type -> "inline function type"
  | `Invalid_result_arity -> "invalid result arity"
  | `Lexer_unknown_operator op -> Fmt.str "unknown operator %s" op
  | `Malformed_utf8_encoding txt -> Fmt.str "malformed UTF-8 encoding %S" txt
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
  | `Trap t -> Fmt.str "trap: %s" (Trap.to_string t)
  | `Type_mismatch msg -> Fmt.str "type mismatch (%s)" msg
  | `Unbound_last_module -> "unbound last module"
  | `Unbound_module id -> Fmt.str "unbound module %s" id
  | `Unbound_name id -> Fmt.str "unbound name %s" id
  | `Undeclared_function_reference -> "undeclared function reference"
  | `Unexpected_token s -> Fmt.str "unexpected token %S" s
  | `Unknown_data id -> Fmt.str "unknown data segment %a" Types.pp_indice id
  | `Unknown_elem id -> Fmt.str "unknown elem segment %a" Types.pp_indice id
  | `Unknown_func id -> Fmt.str "unknown function %a" Types.pp_indice id
  | `Unknown_global id -> Fmt.str "unknown global %a" Types.pp_indice id
  | `Unknown_import (modul, value) -> Fmt.str "unknown import %S %S" modul value
  | `Unknown_label id -> Fmt.str "unknown label %a" Types.pp_indice id
  | `Unknown_local id -> Fmt.str "unknown local %a" Types.pp_indice id
  | `Unknown_memory id -> Fmt.str "unknown memory %a" Types.pp_indice id
  | `Unknown_module name -> Fmt.str "unknown module %s" name
  | `Unknown_operator -> Fmt.str "unknown operator"
  | `Unknown_table id -> Fmt.str "unknown table %a" Types.pp_indice id
  | `Unknown_type id -> Fmt.str "unknown type %a" Types.pp_indice id
  | `Unsupported_file_extension ext ->
    Fmt.str "unsupported file_extension %S" ext
