(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

open Syntax

let token_to_string = function
  | Text_parser.OFFSET -> "offset"
  | I32X4_ADD -> "i32x4.add"
  | I32X4_SUB -> "i32x4.sub"
  | I64X2_ADD -> "i64x2.add"
  | I64X2_SUB -> "i64x2.sub"
  | V128_CONST -> "v128.const"
  | V128 -> "v128"
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
  | I8X16 -> "i8x16"
  | I8 -> "i8"
  | I64_XOR -> "i64.xor"
  | I64_TRUNC_SAT_F64_U -> "i64.trunc_sat_f64_u"
  | I64_TRUNC_SAT_F64_S -> "i64.trunc_sat_f64_s"
  | I64_TRUNC_SAT_F32_U -> "i64.trunc_sat_f32_s"
  | I64_TRUNC_SAT_F32_S -> "i64.trunc_sat_f32_u"
  | I64_TRUNC_F64_U -> "i64.trunc_f64_u"
  | I64_TRUNC_F64_S -> "i64.trunc_f64_s"
  | I64_TRUNC_F32_U -> "i64.trunc_f32_u"
  | I64_TRUNC_F32_S -> "i64.trunc_f32_s"
  | I64_SUB -> "i64.sub"
  | I64_STORE8 -> "i64.store8"
  | I64_STORE32 -> "i64.store32"
  | I64_STORE16 -> "i64.store16"
  | I64_STORE -> "i64.store"
  | I64_SHR_U -> "i64.shr_u"
  | I64_SHR_S -> "i64.shr_s"
  | I64_SHL -> "i64.shl"
  | I64_ROTR -> "i64.rotr"
  | I64_ROTL -> "i64.rotl"
  | I64_REM_U -> "i64.rem_u"
  | I64_REM_S -> "i64.rem_s"
  | I64_REINTERPRET_F64 -> "i64.reinterpret_f64"
  | I64_REINTERPRET_F32 -> "i64.reinterpret_f32"
  | I64_POPCNT -> "i64.popcnt"
  | I64_OR -> "i64.or"
  | I64_NE -> "i64.ne"
  | I64_MUL -> "i64.mul"
  | I64_LT_U -> "i64.lt_u"
  | I64_LT_S -> "i64.lt_s"
  | I64_LOAD8_U -> "i64.load8_u"
  | I64_LOAD8_S -> "i64.load32_u"
  | I64_LOAD32_U -> "i64.load32_u"
  | I64_LOAD32_S -> "i64.load32_s"
  | I64_LOAD16_U -> "i64.load16_u"
  | I64_LOAD16_S -> "i64.load16_s"
  | I64_LOAD -> "i64.load"
  | I64_LE_U -> "i64.le_u"
  | I64_LE_S -> "i64.le_q"
  | I64_GT_U -> "i64.gt_u"
  | I64_GT_S -> "i64.gt_s"
  | I64_GE_U -> "i64.ge_u"
  | I64_GE_S -> "i64.ge_s"
  | I64_EXTEND_I32_U -> "i64.extend_i32_u"
  | I64_EXTEND_I32_S -> "i64.extend_i32_s"
  | I64_EXTEND8_S -> "i64_extend8_s"
  | I64_EXTEND32_S -> "i64.extend32_s"
  | I64_EXTEND16_S -> "i64.extend16_s"
  | I64_EQZ -> "i64.eqz"
  | I64_EQ -> "i64.eq"
  | I64_DIV_U -> "i64.div_u"
  | I64_DIV_S -> "i64.div_s"
  | I64_CTZ -> "i64.ctz"
  | I64_CONST -> "i64.const"
  | I64_CLZ -> "i64.clz"
  | I64_AND -> "i64.and"
  | I64_ADD -> "i64.add"
  | I64X2 -> "i64x2"
  | I64 -> "i64"
  | I32_XOR -> "i32.xor"
  | I32_WRAP_I64 -> "i32.wrap_i64"
  | I32_TRUNC_SAT_F64_U -> "i32.trunc_sat_f64_u"
  | I32_TRUNC_SAT_F64_S -> "i32.trunc_sat_f64_s"
  | I32_TRUNC_SAT_F32_U -> "i32.trunc_sat_f32_u"
  | I32_TRUNC_SAT_F32_S -> "i32.trunc_sat_f32_s"
  | I32_TRUNC_F64_U -> "i32.trunc_f64_u"
  | I32_TRUNC_F64_S -> "i32.trunc_f64_s"
  | I32_TRUNC_F32_U -> "i32.trunc_f32_u"
  | I32_TRUNC_F32_S -> "i32.trunc_f32_s"
  | I32_SUB -> "i32.sub"
  | I32_STORE8 -> "i32.store8"
  | I32_STORE16 -> "i32.store16"
  | I32_STORE -> "i32.store"
  | I32_SHR_U -> "i32.shr_u"
  | I32_SHR_S -> "i32.shr_s"
  | I32_SHL -> "i32.shl"
  | I32_ROTR -> "i32.rotr"
  | I32_ROTL -> "i32.rotl"
  | I32_REM_U -> "i32.rem_u"
  | I32_REM_S -> "i32.rem_s"
  | I32_REINTERPRET_F64 -> "i32.reinterpret_f64"
  | I32_REINTERPRET_F32 -> "i32.reinterpret_f32"
  | I32_POPCNT -> "i32.popcnt"
  | I32_OR -> "i32.or"
  | I32_NE -> "i32.ne"
  | I32_MUL -> "i32.mul"
  | I32_LT_U -> "i32.lt_u"
  | I32_LT_S -> "i32.lt_s"
  | I32_LOAD8_U -> "i32.load8_u"
  | I32_LOAD8_S -> "i32.load8_s"
  | I32_LOAD16_U -> "i32.load16_u"
  | I32_LOAD16_S -> "i32.load16_s"
  | I32_LOAD -> "i32.load"
  | I32_LE_U -> "i32.le_u"
  | I32_LE_S -> "i32.le_s"
  | I32_GT_U -> "i32.gt_u"
  | I32_GT_S -> "i32.gt_s"
  | I32_GE_U -> "i32.ge_u"
  | I32_GE_S -> "i32.ge_s"
  | I32_EXTEND8_S -> "i32.extend8_s"
  | I32_EXTEND16_S -> "i32.extend16_s"
  | I32_EQZ -> "i32.eqz"
  | I32_EQ -> "i32.eq"
  | I32_DIV_U -> "i32.div_u"
  | I32_DIV_S -> "i32.div_s"
  | I32_CTZ -> "i32.ctz"
  | I32_CONST -> "i32.const"
  | I32_CLZ -> "i32.clz"
  | I32_AND -> "i32;and"
  | I32_ADD -> "i32.add"
  | I32X4 -> "i32x4"
  | I32 -> "i32"
  | I31_REF -> "i31.ref"
  | I31_GET_U -> "i31.get_u"
  | I31_GET_S -> "i31.get_s"
  | I31 -> "i31"
  | I16X8 -> "i16x8"
  | I16 -> "i16"
  | GLOBAL_SET -> "global.set"
  | GLOBAL_GET -> "global.get"
  | GLOBAL -> "global"
  | GET -> "get"
  | FUNC_REF -> "func_ref"
  | FUNC -> "func"
  | FINAL -> "final"
  | FIELD -> "field"
  | F64_TRUNC -> "f64.trunc"
  | F64_SUB -> "f64.sub"
  | F64_STORE -> "f64.store"
  | F64_SQRT -> "f64.sqrt"
  | F64_REINTERPRET_I64 -> "f64.reinterpret_i64"
  | F64_REINTERPRET_I32 -> "f64.reinterpret_i32"
  | F64_PROMOTE_F32 -> "f64.promote_f32"
  | F64_NEG -> "f64.neg"
  | F64_NEAREST -> "f64.nearest"
  | F64_NE -> "f64.ne"
  | F64_MUL -> "f64.mul"
  | F64_MIN -> "f64.min"
  | F64_MAX -> "f64.max"
  | F64_LT -> "f64.lt"
  | F64_LOAD -> "f64.load"
  | F64_LE -> "f64.le"
  | F64_GT -> "f64.gt"
  | F64_GE -> "f64.ge"
  | F64_FLOOR -> "f64.floor"
  | F64_EQ -> "f64.eq"
  | F64_DIV -> "f64.div"
  | F64_COPYSIGN -> "f64.copysign"
  | F64_CONVERT_I64_U -> "f64.convert_i64_u"
  | F64_CONVERT_I64_S -> "f64.convert_i64_s"
  | F64_CONVERT_I32_U -> "f64.convert_i32_u"
  | F64_CONVERT_I32_S -> "f64.convert_i32_s"
  | F64_CONST -> "f64.const"
  | F64_CEIL -> "f64.ceil"
  | F64_ADD -> "f64.add"
  | F64_ABS -> "f64.abs"
  | F64X2 -> "f64x2"
  | F64 -> "f64"
  | F32_TRUNC -> "f32.trunc"
  | F32_SUB -> "f32.sub"
  | F32_STORE -> "f32.store"
  | F32_SQRT -> "f32.sqrt"
  | F32_REINTERPRET_I64 -> "f32.reinterpret_i64"
  | F32_REINTERPRET_I32 -> "f32.reinterpret_i32"
  | F32_NEG -> "f32.neg"
  | F32_NEAREST -> "f32.nearest"
  | F32_NE -> "f32.ne"
  | F32_MUL -> "f32.mul"
  | F32_MIN -> "f32.min"
  | F32_MAX -> "f32.max"
  | F32_LT -> "f32.lt"
  | F32_LOAD -> "f32.load"
  | F32_LE -> "f32.le"
  | F32_GT -> "f32.gt"
  | F32_GE -> "f32.ge"
  | F32_FLOOR -> "f32.floor"
  | F32_EQ -> "f32.eq"
  | F32_DIV -> "f32.div"
  | F32_DEMOTE_F64 -> "f32.demote_f64"
  | F32_COPYSIGN -> "f32.copysign"
  | F32_CONVERT_I64_U -> "f32.convert_i64_u"
  | F32_CONVERT_I64_S -> "f32.convert_i64_s"
  | F32_CONVERT_I32_U -> "f32.convert_i32_u"
  | F32_CONVERT_I32_S -> "f32.convert_i32_s"
  | F32_CONST -> "f32.const"
  | F32_CEIL -> "f32.ceil"
  | F32_ADD -> "f32.add"
  | F32_ABS -> "f32.abs"
  | F32X4 -> "f32x4"
  | F32 -> "f32"
  | EXTERN_REF -> "externref"
  | EXTERN_INTERNALIZE -> "extern.internalize"
  | EXTERN_EXTERNALIZE -> "extern.externalize"
  | EXTERN -> "extern"
  | EXPORT -> "export"
  | EQ_REF -> "eqref"
  | EQUAL -> "equal"
  | EQ -> "eq"
  | EOF -> "EOF"
  | END -> "end"
  | ELSE -> "else"
  | ELEM_DROP -> "elem.drop"
  | ELEM -> "elem"
  | DROP -> "drop"
  | DECLARE -> "declare"
  | DATA_DROP -> "data.drop"
  | DATA -> "data"
  | CALL_REF -> "call_ref"
  | CALL_INDIRECT -> "call_indirect"
  | CALL -> "call"
  | BR_TABLE -> "br_table"
  | BR_ON_NULL -> "br_on_null"
  | BR_ON_NON_NULL -> "br_on_non_null"
  | BR_ON_CAST_FAIL -> "br_on_cast_fail"
  | BR_ON_CAST -> "br_on_cast"
  | BR_IF -> "br_if"
  | BR -> "br"
  | BLOCK -> "block"
  | BINARY -> "binary"
  | ASSERT_UNLINKABLE -> "assert_unlinkable"
  | ASSERT_TRAP -> "assert_trap"
  | ASSERT_RETURN -> "assert_return"
  | ASSERT_MALFORMED -> "assert_malformed"
  | ASSERT_INVALID -> "assert_exhaustion"
  | ASSERT_EXHAUSTION -> "assert_exhaustion"
  | ARRAY_SET -> "array.set"
  | ARRAY_REF -> "arrayrref"
  | ARRAY_NEW_CANON_FIXED -> "array.new_canon_fixed"
  | ARRAY_NEW_CANON_ELEM -> "array.new_canon_elem"
  | ARRAY_NEW_CANON_DEFAULT -> "array.new_canon_default"
  | ARRAY_NEW_CANON_DATA -> "array.new_canon_data"
  | ARRAY_NEW_CANON -> "array.new_canon"
  | ARRAY_LEN -> "array.len"
  | ARRAY_GET_U -> "array.get_u"
  | ARRAY_GET -> "array.get"
  | ARRAY -> "array"
  | ANY_REF -> "anyref"
  | ANY -> "any"
  | ALIGN -> "align"
  | NUM s -> Fmt.str "%s" s
  | NAME s -> Fmt.str {|"%s"|} s
  | ID s -> Fmt.str "$%s" s

module Make (M : sig
  type t

  val rule : (Lexing.lexbuf -> Text_parser.token) -> Lexing.lexbuf -> t
end) =
struct
  let pp_pos fmt ((p1, p2) : Lexing.position * Lexing.position) =
    if p1.pos_lnum = p2.pos_lnum then
      if p1.pos_cnum = p2.pos_cnum then
        Fmt.pf fmt "line %d, character %d" p1.pos_lnum (p1.pos_cnum - p1.pos_bol)
      else
        Fmt.pf fmt "line %d, character %d-%d" p1.pos_lnum
          (p1.pos_cnum - p1.pos_bol) (p2.pos_cnum - p2.pos_bol)
    else
      Fmt.pf fmt "line %d, character %d to line %d, character %d" p1.pos_lnum
        (p1.pos_cnum - p1.pos_bol) p2.pos_lnum (p2.pos_cnum - p2.pos_bol)

  let from_lexbuf =
    let parser = MenhirLib.Convert.Simplified.traditional2revised M.rule in
    fun buf ->
      Log.info (fun m -> m "parsing      ...");
      let provider () =
        let tok = Text_lexer.token buf in
        let start, stop = Sedlexing.lexing_positions buf in
        (tok, start, stop)
      in
      try Ok (parser provider) with
      | Text.Parse_fail msg -> Error (`Parse_fail msg)
      | Text_lexer.Empty_annotation_id -> Error `Empty_annotation_id
      | Text_lexer.Empty_identifier -> Error `Empty_identifier
      | Text_lexer.Illegal_escape msg -> Error (`Illegal_escape msg)
      | Text_lexer.Illegal_character msg -> Error (`Lexer_illegal_character msg)
      | Text_lexer.Unclosed_annotation -> Error `Unclosed_annotation
      | Text_lexer.Unclosed_comment -> Error `Unclosed_comment
      | Text_lexer.Unclosed_string -> Error `Unclosed_string
      | Text_lexer.Unknown_operator msg -> Error (`Lexer_unknown_operator msg)
      | Text_parser.Error ->
        let tok, pos1, pos2 = Text_lexer.lexer buf () in
        let msg =
          Fmt.str "%s in %a" (token_to_string tok) pp_pos (pos1, pos2)
        in
        Error (`Unexpected_token msg)
      | Sedlexing.MalFormed -> Error (`Malformed_utf8_encoding "")

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
    type t = Wast.script

    let rule = Text_parser.script
  end)

  module Module = Make (struct
    type t = Text.Module.t

    let rule = Text_parser.modul
  end)

  module Inline_module = Make (struct
    type t = Text.Module.t

    let rule = Text_parser.inline_module
  end)
end

module Binary = struct
  module Module = Binary_parser
end

let guess_from_file file =
  Log.bench_fn "parsing time" @@ fun () ->
  match Fpath.get_ext ~multi:false file with
  | ".wat" ->
    let+ m = Text.Module.from_file file in
    Kind.Wat m
  | ".wast" ->
    let+ m = Text.Script.from_file file in
    Kind.Wast m
  | ".wasm" ->
    let+ m = Binary.Module.from_file file in
    Kind.Wasm m
  | ext -> Error (`Unsupported_file_extension ext)
