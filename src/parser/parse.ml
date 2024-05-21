(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

open Syntax

let token_to_string = function
  | Text_parser.OFFSET -> "offset"
  | UNREACHABLE -> "unreachable"
  | TYPE -> "type"
  | THEN -> "then"
  | TABLE_SIZE -> "table.size"
  | TABLE_SET -> "table.set"
  | TABLE_INIT -> "table.init"
  | TABLE_GROW -> "table.grow"
  | TABLE_GET -> "table.get"
  | TABLE_FILL -> "table.fill"
  | TABLE_COPY -> "table.copy"
  | TABLE -> "table"
  | SUB -> "sub"
  | STRUCT_SET -> "struct.set"
  | STRUCT_NEW_CANON_DEFAULT -> "struct.new_canon_default"
  | STRUCT_NEW_CANON -> "struct.new_canon"
  | STRUCT_GET_S -> "struct.get_s"
  | STRUCT_GET -> "struct.get"
  | STRUCTREF -> "structref"
  | STRUCT -> "struct"
  | START -> "start"
  | SELECT -> "select"
  | RPAR -> ")"
  | RETURN_CALL_REF -> "return_call_ref"
  | RETURN_CALL_INDIRECT -> "return_call_indirect"
  | RETURN_CALL -> "return_call"
  | RETURN -> "return"
  | RESULT -> "result"
  | REGISTER -> "register"
  | REF_TEST -> "ref.test"
  | REF_STRUCT -> "ref.struct"
  | REF_NULL -> "ref.null"
  | REF_IS_NULL -> "ref.is_null"
  | REF_I31 -> "ref.i31"
  | REF_HOST -> "ref.host"
  | REF_FUNC -> "ref.func"
  | REF_EXTERN -> "ref.extern"
  | REF_EQ -> "ref.eq"
  | REF_CAST -> "ref.cast"
  | REF_AS_NON_NULL -> "ref.as_non_null"
  | REF_ARRAY -> "ref.array"
  | REF -> "ref"
  | REC -> "rec"
  | QUOTE -> "quote"
  | PARAM -> "param"
  | NULL_REF -> "null.ref"
  | NULL_FUNC_REF -> "null.func_ref"
  | NULL_EXTERN_REF -> "null.extern_ref"
  | NULL -> "null"
  | NOP -> "nop"
  | NONE -> "none"
  | NOFUNC -> "nofunc"
  | NOEXTERN -> "noextern"
  | NAN_CANON -> "nan_canon"
  | NAN_ARITH -> "nan_arith"
  | MUTABLE -> "mutable"
  | MODULE -> "module"
  | MEMORY_SIZE -> "memory.size"
  | MEMORY_INIT -> "memory.init"
  | MEMORY_GROW -> "memory.grow"
  | MEMORY_FILL -> "memory.fill"
  | MEMORY_COPY -> "memory.copy"
  | MEMORY -> "memory"
  | LPAR -> "("
  | LOOP -> "loop"
  | LOCAL_TEE -> "local.tee"
  | LOCAL_SET -> "local.set"
  | LOCAL_GET -> "local.get"
  | LOCAL -> "local"
  | ITEM -> "item"
  | INVOKE -> "invoke"
  | IMPORT -> "import"
  | IF -> "if"
  | I8 -> "i8"
  | I64_XOR -> assert false
  | I64_TRUNC_SAT_F64_U -> assert false
  | I64_TRUNC_SAT_F64_S -> assert false
  | I64_TRUNC_SAT_F32_U -> assert false
  | I64_TRUNC_SAT_F32_S -> assert false
  | I64_TRUNC_F64_U -> assert false
  | I64_TRUNC_F64_S -> assert false
  | I64_TRUNC_F32_U -> assert false
  | I64_TRUNC_F32_S -> assert false
  | I64_SUB -> assert false
  | I64_STORE8 -> assert false
  | I64_STORE32 -> assert false
  | I64_STORE16 -> assert false
  | I64_STORE -> assert false
  | I64_SHR_U -> assert false
  | I64_SHR_S -> assert false
  | I64_SHL -> assert false
  | I64_ROTR -> assert false
  | I64_ROTL -> assert false
  | I64_REM_U -> assert false
  | I64_REM_S -> assert false
  | I64_REINTERPRET_F64 -> assert false
  | I64_REINTERPRET_F32 -> assert false
  | I64_POPCNT -> assert false
  | I64_OR -> assert false
  | I64_NE -> assert false
  | I64_MUL -> assert false
  | I64_LT_U -> assert false
  | I64_LT_S -> assert false
  | I64_LOAD8_U -> assert false
  | I64_LOAD8_S -> assert false
  | I64_LOAD32_U -> assert false
  | I64_LOAD32_S -> assert false
  | I64_LOAD16_U -> assert false
  | I64_LOAD16_S -> assert false
  | I64_LOAD -> assert false
  | I64_LE_U -> assert false
  | I64_LE_S -> assert false
  | I64_GT_U -> assert false
  | I64_GT_S -> assert false
  | I64_GE_U -> assert false
  | I64_GE_S -> assert false
  | I64_EXTEND_I32_U -> assert false
  | I64_EXTEND_I32_S -> assert false
  | I64_EXTEND8_S -> assert false
  | I64_EXTEND32_S -> assert false
  | I64_EXTEND16_S -> assert false
  | I64_EQZ -> assert false
  | I64_EQ -> assert false
  | I64_DIV_U -> assert false
  | I64_DIV_S -> assert false
  | I64_CTZ -> assert false
  | I64_CONST -> assert false
  | I64_CLZ -> assert false
  | I64_AND -> assert false
  | I64_ADD -> assert false
  | I64 -> assert false
  | I32_XOR -> assert false
  | I32_WRAP_I64 -> assert false
  | I32_TRUNC_SAT_F64_U -> assert false
  | I32_TRUNC_SAT_F64_S -> assert false
  | I32_TRUNC_SAT_F32_U -> assert false
  | I32_TRUNC_SAT_F32_S -> assert false
  | I32_TRUNC_F64_U -> assert false
  | I32_TRUNC_F64_S -> assert false
  | I32_TRUNC_F32_U -> assert false
  | I32_TRUNC_F32_S -> assert false
  | I32_SUB -> assert false
  | I32_STORE8 -> assert false
  | I32_STORE16 -> assert false
  | I32_STORE -> assert false
  | I32_SHR_U -> assert false
  | I32_SHR_S -> assert false
  | I32_SHL -> assert false
  | I32_ROTR -> assert false
  | I32_ROTL -> assert false
  | I32_REM_U -> assert false
  | I32_REM_S -> assert false
  | I32_REINTERPRET_F64 -> assert false
  | I32_REINTERPRET_F32 -> assert false
  | I32_POPCNT -> assert false
  | I32_OR -> assert false
  | I32_NE -> assert false
  | I32_MUL -> assert false
  | I32_LT_U -> assert false
  | I32_LT_S -> assert false
  | I32_LOAD8_U -> assert false
  | I32_LOAD8_S -> assert false
  | I32_LOAD16_U -> assert false
  | I32_LOAD16_S -> assert false
  | I32_LOAD -> assert false
  | I32_LE_U -> assert false
  | I32_LE_S -> assert false
  | I32_GT_U -> assert false
  | I32_GT_S -> assert false
  | I32_GE_U -> assert false
  | I32_GE_S -> assert false
  | I32_EXTEND8_S -> assert false
  | I32_EXTEND16_S -> assert false
  | I32_EQZ -> assert false
  | I32_EQ -> assert false
  | I32_DIV_U -> assert false
  | I32_DIV_S -> assert false
  | I32_CTZ -> assert false
  | I32_CONST -> assert false
  | I32_CLZ -> assert false
  | I32_AND -> assert false
  | I32_ADD -> assert false
  | I32 -> assert false
  | I31_REF -> assert false
  | I31_GET_U -> assert false
  | I31_GET_S -> assert false
  | I31 -> assert false
  | I16 -> assert false
  | GLOBAL_SET -> assert false
  | GLOBAL_GET -> assert false
  | GLOBAL -> assert false
  | GET -> assert false
  | FUNC_REF -> assert false
  | FUNC -> assert false
  | FINAL -> assert false
  | FIELD -> assert false
  | F64_TRUNC -> assert false
  | F64_SUB -> assert false
  | F64_STORE -> assert false
  | F64_SQRT -> assert false
  | F64_REINTERPRET_I64 -> assert false
  | F64_REINTERPRET_I32 -> assert false
  | F64_PROMOTE_F32 -> assert false
  | F64_NEG -> assert false
  | F64_NEAREST -> assert false
  | F64_NE -> assert false
  | F64_MUL -> assert false
  | F64_MIN -> assert false
  | F64_MAX -> assert false
  | F64_LT -> assert false
  | F64_LOAD -> assert false
  | F64_LE -> assert false
  | F64_GT -> assert false
  | F64_GE -> assert false
  | F64_FLOOR -> assert false
  | F64_EQ -> assert false
  | F64_DIV -> assert false
  | F64_COPYSIGN -> assert false
  | F64_CONVERT_I64_U -> assert false
  | F64_CONVERT_I64_S -> assert false
  | F64_CONVERT_I32_U -> assert false
  | F64_CONVERT_I32_S -> assert false
  | F64_CONST -> assert false
  | F64_CEIL -> assert false
  | F64_ADD -> assert false
  | F64_ABS -> assert false
  | F64 -> assert false
  | F32_TRUNC -> assert false
  | F32_SUB -> assert false
  | F32_STORE -> assert false
  | F32_SQRT -> assert false
  | F32_REINTERPRET_I64 -> assert false
  | F32_REINTERPRET_I32 -> assert false
  | F32_NEG -> assert false
  | F32_NEAREST -> assert false
  | F32_NE -> assert false
  | F32_MUL -> assert false
  | F32_MIN -> assert false
  | F32_MAX -> assert false
  | F32_LT -> assert false
  | F32_LOAD -> assert false
  | F32_LE -> assert false
  | F32_GT -> assert false
  | F32_GE -> assert false
  | F32_FLOOR -> assert false
  | F32_EQ -> assert false
  | F32_DIV -> assert false
  | F32_DEMOTE_F64 -> assert false
  | F32_COPYSIGN -> assert false
  | F32_CONVERT_I64_U -> assert false
  | F32_CONVERT_I64_S -> assert false
  | F32_CONVERT_I32_U -> assert false
  | F32_CONVERT_I32_S -> assert false
  | F32_CONST -> assert false
  | F32_CEIL -> assert false
  | F32_ADD -> assert false
  | F32_ABS -> assert false
  | F32 -> assert false
  | EXTERN_REF -> assert false
  | EXTERN_INTERNALIZE -> assert false
  | EXTERN_EXTERNALIZE -> assert false
  | EXTERN -> assert false
  | EXPORT -> assert false
  | EQ_REF -> assert false
  | EQUAL -> assert false
  | EQ -> assert false
  | EOF -> assert false
  | END -> assert false
  | ELSE -> assert false
  | ELEM_DROP -> assert false
  | ELEM -> assert false
  | DROP -> assert false
  | DECLARE -> assert false
  | DATA_DROP -> assert false
  | DATA -> assert false
  | CALL_REF -> assert false
  | CALL_INDIRECT -> assert false
  | CALL -> assert false
  | BR_TABLE -> assert false
  | BR_ON_NULL -> assert false
  | BR_ON_NON_NULL -> assert false
  | BR_ON_CAST_FAIL -> assert false
  | BR_ON_CAST -> assert false
  | BR_IF -> assert false
  | BR -> assert false
  | BLOCK -> assert false
  | BINARY -> assert false
  | ASSERT_UNLINKABLE -> assert false
  | ASSERT_TRAP -> assert false
  | ASSERT_RETURN -> assert false
  | ASSERT_MALFORMED -> assert false
  | ASSERT_INVALID -> assert false
  | ASSERT_EXHAUSTION -> assert false
  | ARRAY_SET -> assert false
  | ARRAY_REF -> assert false
  | ARRAY_NEW_CANON_FIXED -> assert false
  | ARRAY_NEW_CANON_ELEM -> assert false
  | ARRAY_NEW_CANON_DEFAULT -> assert false
  | ARRAY_NEW_CANON_DATA -> assert false
  | ARRAY_NEW_CANON -> assert false
  | ARRAY_LEN -> assert false
  | ARRAY_GET_U -> assert false
  | ARRAY_GET -> assert false
  | ARRAY -> "array"
  | ANY_REF -> "anyref"
  | ANY -> "any"
  | ALIGN -> "align"
  | NUM s -> Format.sprintf "%s" s
  | NAME s -> Format.sprintf {|"%s"|} s
  | ID s -> Format.sprintf "$%s" s

module Make (M : sig
  type t

  val rule : (Lexing.lexbuf -> Text_parser.token) -> Lexing.lexbuf -> t
end) =
struct
  let from_lexbuf =
    let parser = MenhirLib.Convert.Simplified.traditional2revised M.rule in
    fun buf ->
      Log.debug0 "parsing      ...@\n";
      let provider () =
        let tok = Text_lexer.token buf in
        let start, stop = Sedlexing.lexing_positions buf in
        (tok, start, stop)
      in
      try Ok (parser provider) with
      | Types.Parse_fail msg -> Error (`Parse_fail msg)
      | Text_lexer.Illegal_escape msg -> Error (`Illegal_escape msg)
      | Text_lexer.Unknown_operator msg -> Error (`Lexer_unknown_operator msg)
      | Text_lexer.Unexpected_character msg ->
        Error (`Lexer_unknown_operator msg)
      | Text_parser.Error ->
        let tok = Text_lexer.token buf |> token_to_string in
        Error (`Unexpected_token tok)

  let from_file filename =
    let open Syntax in
    let* res =
      Bos.OS.File.with_ic filename
        (fun chan () ->
          let lb = Sedlexing.Utf8.from_channel chan in
          Sedlexing.set_filename lb (Fpath.to_string filename);
          from_lexbuf lb )
        ()
    in
    res

  let from_string s = from_lexbuf (Sedlexing.Utf8.from_string s)

  let from_channel c = from_lexbuf (Sedlexing.Utf8.from_channel c)
end

module Text = struct
  module Script = Make (struct
    type t = Text.script

    let rule = Text_parser.script
  end)

  module Module = Make (struct
    type t = Text.modul

    let rule = Text_parser.modul
  end)

  module Inline_module = Make (struct
    type t = Text.modul

    let rule = Text_parser.inline_module
  end)
end

module Binary = struct
  module Module = Binary_parser
end

let guess_from_file file =
  match Fpath.get_ext ~multi:false file with
  | ".wat" ->
    let+ m = Text.Module.from_file file in
    Either.Left (Either.Left m)
  | ".wast" ->
    let+ m = Text.Script.from_file file in
    Either.Left (Either.Right m)
  | ".wasm" ->
    let+ m = Binary.Module.from_file file in
    Either.Right m
  | ext -> Error (`Unsupported_file_extension ext)
