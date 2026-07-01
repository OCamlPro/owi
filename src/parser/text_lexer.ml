(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Sedlexing
open Text_parser

exception Empty_annotation_id

exception Empty_identifier

exception Illegal_character of string

exception Illegal_escape of string

exception Unclosed_annotation

exception Unclosed_comment

exception Unclosed_string

exception Unknown_operator of string

let illegal_character buf =
  let tok = Utf8.lexeme buf in
  raise @@ Illegal_character (Fmt.str "illegal character %S" tok)

let illegal_escape buf =
  let tok = Utf8.lexeme buf in
  raise @@ Illegal_escape (Fmt.str "illegal escape %S" tok)

let unknown_operator buf =
  let tok = Utf8.lexeme buf in
  raise @@ Unknown_operator (Fmt.str "unknown operator %S" tok)

let mk_string buf s =
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    let c =
      if not @@ Char.equal s.[!i] '\\' then s.[!i]
      else
        match
          incr i;
          s.[!i]
        with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | '\\' -> '\\'
        | '\'' -> '\''
        | '\"' -> '\"'
        | 'u' ->
          let j = !i + 2 in
          begin match String.index_from_opt s j '}' with
          | None -> (* TODO: is this the expected error ? *) illegal_escape buf
          | Some index ->
            i := index;
            let n =
              int_of_string_opt (Fmt.str "0x%s" (String.sub s j (!i - j)))
            in
            let n = match n with None -> assert false | Some n -> n in
            let bs = Wutf8.encode [ n ] in
            Buffer.add_substring b bs 0 (String.length bs - 1);
            bs.[String.length bs - 1]
          end
        | h ->
          incr i;
          if !i >= String.length s then illegal_escape buf;
          let str = Fmt.str "0x%c%c" h s.[!i] in
          begin match int_of_string_opt str with
          | None -> illegal_escape buf
          | Some n -> Char.chr n
          end
    in
    Buffer.add_char b c;
    incr i
  done;
  Buffer.contents b

let blank = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let any_blank = [%sedlex.regexp? blank | newline]

let sign = [%sedlex.regexp? '+' | '-']

let digit = [%sedlex.regexp? '0' .. '9']

let hexdigit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let num = [%sedlex.regexp? digit, Star (Opt '_', digit)]

let hexnum = [%sedlex.regexp? hexdigit, Star (Opt '_', hexdigit)]

let hexfrac = [%sedlex.regexp? hexnum]

let frac = [%sedlex.regexp? num]

let float =
  [%sedlex.regexp?
    ( Opt sign, num, '.', Opt frac
    | Opt sign, num, Opt ('.', Opt frac), ('e' | 'E'), Opt sign, num
    | Opt sign, "0x", hexnum, '.', Opt hexfrac
    | Opt sign, "0x", hexnum, Opt ('.', Opt hexfrac), ('p' | 'P'), Opt sign, num
    | Opt sign, "inf"
    | Opt sign, "nan"
    | Opt sign, "nan:", "0x", hexnum )]

let nat = [%sedlex.regexp? num | "0x", hexnum]

let int = [%sedlex.regexp? sign, nat]

let num = [%sedlex.regexp? float | int | nat]

let id_char =
  [%sedlex.regexp?
    ( '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/' | ':'
    | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~' )]

let string_elem = [%sedlex.regexp? Sub (any, "\"") | "\\\""]

let name = [%sedlex.regexp? "\"", Star string_elem, "\""]

let operator =
  [%sedlex.regexp? Plus ('0' .. '9' | 'a' .. 'z' | '.' | '_' | ':'), Star name]

let id = [%sedlex.regexp? "$", Plus id_char]

let bad_name = [%sedlex.regexp? name, Plus (name | id | operator)]

let bad_id = [%sedlex.regexp? id, Plus name]

let bad_num = [%sedlex.regexp? num, Plus id]

let annot_atom =
  [%sedlex.regexp?
    Plus id_char | num | name | ',' | ';' | '[' | ']' | '{' | '}']

let keywords =
  let tbl = Hashtbl.create 512 in
  Array.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [| ("align", ALIGN)
     ; ("any", ANY)
     ; ("any.convert_extern", ANY_CONVERT_EXTERN)
     ; ("anyref", ANY_REF)
     ; ("array", ARRAY)
     ; ("array.copy", ARRAY_COPY)
     ; ("array.fill", ARRAY_FILL)
     ; ("array.get", ARRAY_GET)
     ; ("array.get_s", ARRAY_GET_S)
     ; ("array.get_u", ARRAY_GET_U)
     ; ("array.init_data", ARRAY_INIT_DATA)
     ; ("array.init_elem", ARRAY_INIT_ELEM)
     ; ("array.len", ARRAY_LEN)
     ; ("array.new", ARRAY_NEW_CANON)
     ; ("array.new_data", ARRAY_NEW_CANON_DATA)
     ; ("array.new_default", ARRAY_NEW_CANON_DEFAULT)
     ; ("array.new_elem", ARRAY_NEW_CANON_ELEM)
     ; ("array.new_fixed", ARRAY_NEW_CANON_FIXED)
     ; ("arrayref", ARRAY_REF)
     ; ("array.set", ARRAY_SET)
     ; ("assert_exhaustion", ASSERT_EXHAUSTION)
     ; ("assert_invalid", ASSERT_INVALID)
     ; ("assert_malformed", ASSERT_MALFORMED)
     ; ("assert_return", ASSERT_RETURN)
     ; ("assert_trap", ASSERT_TRAP)
     ; ("assert_unlinkable", ASSERT_UNLINKABLE)
     ; ("binary", BINARY)
     ; ("block", BLOCK)
     ; ("br", BR)
     ; ("br_if", BR_IF)
     ; ("br_on_cast", BR_ON_CAST)
     ; ("br_on_cast_fail", BR_ON_CAST_FAIL)
     ; ("br_on_non_null", BR_ON_NON_NULL)
     ; ("br_on_null", BR_ON_NULL)
     ; ("br_table", BR_TABLE)
     ; ("call", CALL)
     ; ("call_indirect", CALL_INDIRECT)
     ; ("call_ref", CALL_REF)
     ; ("data", DATA)
     ; ("data.drop", DATA_DROP)
     ; ("declare", DECLARE)
     ; ("definition", DEFINITION)
     ; ("drop", DROP)
     ; ("elem.drop", ELEM_DROP)
     ; ("elem", ELEM)
     ; ("else", ELSE)
     ; ("end", END)
     ; ("eq", EQ)
     ; ("eqref", EQ_REF)
     ; ("exn", EXN)
     ; ("exnref", EXN_REF)
     ; ("export", EXPORT)
     ; ("extern.convert_any", EXTERN_CONVERT_ANY)
     ; ("extern", EXTERN)
     ; ("externref", EXTERN_REF)
     ; ("f32.abs", F32_ABS)
     ; ("f32.add", F32_ADD)
     ; ("f32.ceil", F32_CEIL)
     ; ("f32.const", F32_CONST)
     ; ("f32.convert_i32_s", F32_CONVERT_I32_S)
     ; ("f32.convert_i32_u", F32_CONVERT_I32_U)
     ; ("f32.convert_i64_s", F32_CONVERT_I64_S)
     ; ("f32.convert_i64_u", F32_CONVERT_I64_U)
     ; ("f32.copysign", F32_COPYSIGN)
     ; ("f32.demote_f64", F32_DEMOTE_F64)
     ; ("f32.div", F32_DIV)
     ; ("f32.eq", F32_EQ)
     ; ("f32", F32)
     ; ("f32.floor", F32_FLOOR)
     ; ("f32.ge", F32_GE)
     ; ("f32.gt", F32_GT)
     ; ("f32.le", F32_LE)
     ; ("f32.load", F32_LOAD)
     ; ("f32.lt", F32_LT)
     ; ("f32.max", F32_MAX)
     ; ("f32.min", F32_MIN)
     ; ("f32.mul", F32_MUL)
     ; ("f32.nearest", F32_NEAREST)
     ; ("f32.ne", F32_NE)
     ; ("f32.neg", F32_NEG)
     ; ("f32.reinterpret_i32", F32_REINTERPRET_I32)
     ; ("f32.reinterpret_i64", F32_REINTERPRET_I64)
     ; ("f32.sqrt", F32_SQRT)
     ; ("f32.store", F32_STORE)
     ; ("f32.sub", F32_SUB)
     ; ("f32.trunc", F32_TRUNC)
     ; ("f32x4.add", F32X4_ADD)
     ; ("f32x4.ceil", F32X4_CEIL)
     ; ("f32x4.convert_i32x4_s", F32X4_CONVERT_I32X4_S)
     ; ("f32x4.eq", F32X4_EQ)
     ; ("f32x4", F32X4)
     ; ("f32x4.min", F32X4_MIN)
     ; ("f32x4.pmin", F32X4_PMIN)
     ; ("f64.abs", F64_ABS)
     ; ("f64.add", F64_ADD)
     ; ("f64.ceil", F64_CEIL)
     ; ("f64.const", F64_CONST)
     ; ("f64.convert_i32_s", F64_CONVERT_I32_S)
     ; ("f64.convert_i32_u", F64_CONVERT_I32_U)
     ; ("f64.convert_i64_s", F64_CONVERT_I64_S)
     ; ("f64.convert_i64_u", F64_CONVERT_I64_U)
     ; ("f64.copysign", F64_COPYSIGN)
     ; ("f64.div", F64_DIV)
     ; ("f64.eq", F64_EQ)
     ; ("f64", F64)
     ; ("f64.floor", F64_FLOOR)
     ; ("f64.ge", F64_GE)
     ; ("f64.gt", F64_GT)
     ; ("f64.le", F64_LE)
     ; ("f64.load", F64_LOAD)
     ; ("f64.lt", F64_LT)
     ; ("f64.max", F64_MAX)
     ; ("f64.min", F64_MIN)
     ; ("f64.mul", F64_MUL)
     ; ("f64.nearest", F64_NEAREST)
     ; ("f64.ne", F64_NE)
     ; ("f64.neg", F64_NEG)
     ; ("f64.promote_f32", F64_PROMOTE_F32)
     ; ("f64.reinterpret_i32", F64_REINTERPRET_I32)
     ; ("f64.reinterpret_i64", F64_REINTERPRET_I64)
     ; ("f64.sqrt", F64_SQRT)
     ; ("f64.store", F64_STORE)
     ; ("f64.sub", F64_SUB)
     ; ("f64.trunc", F64_TRUNC)
     ; ("f64x2.add", F64X2_ADD)
     ; ("f64x2.ceil", F64X2_CEIL)
     ; ("f64x2.eq", F64X2_EQ)
     ; ("f64x2", F64X2)
     ; ("f64x2.min", F64X2_MIN)
     ; ("f64x2.pmin", F64X2_PMIN)
     ; ("field", FIELD)
     ; ("final", FINAL)
     ; ("func", FUNC)
     ; ("funcref", FUNC_REF)
     ; ("get", GET)
     ; ("global.get", GLOBAL_GET)
     ; ("global", GLOBAL)
     ; ("global.set", GLOBAL_SET)
     ; ("i16", I16)
     ; ("i16x8.add", I16X8_ADD)
     ; ("i16x8.add_sat_s", I16X8_ADD_SAT_S)
     ; ("i16x8.eq", I16X8_EQ)
     ; ("i16x8.extadd_pairwise_i8x16_s", I16X8_EXTADD_PAIRWISE_I8X16_S)
     ; ("i16x8.extend_high_i8x16_s", I16X8_EXTEND_HIGH_I8X16_S)
     ; ("i16x8_extend_high_i8x16_s", I16X8_EXTEND_HIGH_I8X16_S)
     ; ("i16x8.extmul_low_i8x16_s", I16X8_EXTMUL_LOW_I8X16_S)
     ; ("i16x8", I16X8)
     ; ("i16x8.min_s", I16X8_MIN_S)
     ; ("i16x8.q15mulr_sat_s", I16X8_Q15MULR_SAT_S)
     ; ("i31.get_s", I31_GET_S)
     ; ("i31.get_u", I31_GET_U)
     ; ("i31", I31)
     ; ("i31ref", I31_REF)
     ; ("i32.add", I32_ADD)
     ; ("i32.and", I32_AND)
     ; ("i32.clz", I32_CLZ)
     ; ("i32.const", I32_CONST)
     ; ("i32.ctz", I32_CTZ)
     ; ("i32.div_s", I32_DIV_S)
     ; ("i32.div_u", I32_DIV_U)
     ; ("i32.eq", I32_EQ)
     ; ("i32.eqz", I32_EQZ)
     ; ("i32.extend16_s", I32_EXTEND16_S)
     ; ("i32.extend8_s", I32_EXTEND8_S)
     ; ("i32.ge_s", I32_GE_S)
     ; ("i32.ge_u", I32_GE_U)
     ; ("i32.gt_s", I32_GT_S)
     ; ("i32.gt_u", I32_GT_U)
     ; ("i32", I32)
     ; ("i32.le_s", I32_LE_S)
     ; ("i32.le_u", I32_LE_U)
     ; ("i32.load16_s", I32_LOAD16_S)
     ; ("i32.load16_u", I32_LOAD16_U)
     ; ("i32.load8_s", I32_LOAD8_S)
     ; ("i32.load8_u", I32_LOAD8_U)
     ; ("i32.load", I32_LOAD)
     ; ("i32.lt_s", I32_LT_S)
     ; ("i32.lt_u", I32_LT_U)
     ; ("i32.mul", I32_MUL)
     ; ("i32.ne", I32_NE)
     ; ("i32.or", I32_OR)
     ; ("i32.popcnt", I32_POPCNT)
     ; ("i32.reinterpret_f32", I32_REINTERPRET_F32)
     ; ("i32.reinterpret_f64", I32_REINTERPRET_F64)
     ; ("i32.rem_s", I32_REM_S)
     ; ("i32.rem_u", I32_REM_U)
     ; ("i32.rotl", I32_ROTL)
     ; ("i32.rotr", I32_ROTR)
     ; ("i32.shl", I32_SHL)
     ; ("i32.shr_s", I32_SHR_S)
     ; ("i32.shr_u", I32_SHR_U)
     ; ("i32.store16", I32_STORE16)
     ; ("i32.store8", I32_STORE8)
     ; ("i32.store", I32_STORE)
     ; ("i32.sub", I32_SUB)
     ; ("i32.trunc_f32_s", I32_TRUNC_F32_S)
     ; ("i32.trunc_f32_u", I32_TRUNC_F32_U)
     ; ("i32.trunc_f64_s", I32_TRUNC_F64_S)
     ; ("i32.trunc_f64_u", I32_TRUNC_F64_U)
     ; ("i32.trunc_sat_f32_s", I32_TRUNC_SAT_F32_S)
     ; ("i32.trunc_sat_f32_u", I32_TRUNC_SAT_F32_U)
     ; ("i32.trunc_sat_f64_s", I32_TRUNC_SAT_F64_S)
     ; ("i32.trunc_sat_f64_u", I32_TRUNC_SAT_F64_U)
     ; ("i32.wrap_i64", I32_WRAP_I64)
     ; ("i32x4.add", I32X4_ADD)
     ; ("i32x4.dot_i16x8_s", I32X4_DOT_I16X8_S)
     ; ("i32x4.eq", I32X4_EQ)
     ; ("i32x4.extadd_pairwise_i16x8_s", I32X4_EXTADD_PAIRWISE_I16X8_S)
     ; ("i32x4.extmul_low_i16x8_s", I32X4_EXTMUL_LOW_I16X8_S)
     ; ("i32x4", I32X4)
     ; ("i32x4.min_s", I32X4_MIN_S)
     ; ("i32x4.mul", I32X4_MUL)
     ; ("i32x4.sub", I32X4_SUB)
     ; ("i32x4.trunc_sat_f32x4_s", I32X4_TRUNC_SAT_F32X4_S)
     ; ("i32x4.trunc_sat_f64x2_s_zero", I32X4_TRUNC_SAT_F64X2_S_ZERO)
     ; ("i32x4.trunct_sat_f32x4_s_zero", I32X4_TRUNC_SAT_F32X4_S_ZERO)
     ; ("i32.xor", I32_XOR)
     ; ("i64.add", I64_ADD)
     ; ("i64.and", I64_AND)
     ; ("i64.clz", I64_CLZ)
     ; ("i64.const", I64_CONST)
     ; ("i64.ctz", I64_CTZ)
     ; ("i64.div_s", I64_DIV_S)
     ; ("i64.div_u", I64_DIV_U)
     ; ("i64.eq", I64_EQ)
     ; ("i64.eqz", I64_EQZ)
     ; ("i64.extend16_s", I64_EXTEND16_S)
     ; ("i64.extend32_s", I64_EXTEND32_S)
     ; ("i64.extend8_s", I64_EXTEND8_S)
     ; ("i64.extend_i32_s", I64_EXTEND_I32_S)
     ; ("i64.extend_i32_u", I64_EXTEND_I32_U)
     ; ("i64.ge_s", I64_GE_S)
     ; ("i64.ge_u", I64_GE_U)
     ; ("i64.gt_s", I64_GT_S)
     ; ("i64.gt_u", I64_GT_U)
     ; ("i64", I64)
     ; ("i64.le_s", I64_LE_S)
     ; ("i64.le_u", I64_LE_U)
     ; ("i64.load16_s", I64_LOAD16_S)
     ; ("i64.load16_u", I64_LOAD16_U)
     ; ("i64.load32_s", I64_LOAD32_S)
     ; ("i64.load32_u", I64_LOAD32_U)
     ; ("i64.load8_s", I64_LOAD8_S)
     ; ("i64.load8_u", I64_LOAD8_U)
     ; ("i64.load", I64_LOAD)
     ; ("i64.lt_s", I64_LT_S)
     ; ("i64.lt_u", I64_LT_U)
     ; ("i64.mul", I64_MUL)
     ; ("i64.ne", I64_NE)
     ; ("i64.or", I64_OR)
     ; ("i64.popcnt", I64_POPCNT)
     ; ("i64.reinterpret_f32", I64_REINTERPRET_F32)
     ; ("i64.reinterpret_f64", I64_REINTERPRET_F64)
     ; ("i64.rem_s", I64_REM_S)
     ; ("i64.rem_u", I64_REM_U)
     ; ("i64.rotl", I64_ROTL)
     ; ("i64.rotr", I64_ROTR)
     ; ("i64.shl", I64_SHL)
     ; ("i64.shr_s", I64_SHR_S)
     ; ("i64.shr_u", I64_SHR_U)
     ; ("i64.store16", I64_STORE16)
     ; ("i64.store32", I64_STORE32)
     ; ("i64.store8", I64_STORE8)
     ; ("i64.store", I64_STORE)
     ; ("i64.sub", I64_SUB)
     ; ("i64.trunc_f32_s", I64_TRUNC_F32_S)
     ; ("i64.trunc_f32_u", I64_TRUNC_F32_U)
     ; ("i64.trunc_f64_s", I64_TRUNC_F64_S)
     ; ("i64.trunc_f64_u", I64_TRUNC_F64_U)
     ; ("i64.trunc_sat_f32_s", I64_TRUNC_SAT_F32_S)
     ; ("i64.trunc_sat_f32_u", I64_TRUNC_SAT_F32_U)
     ; ("i64.trunc_sat_f64_s", I64_TRUNC_SAT_F64_S)
     ; ("i64.trunc_sat_f64_u", I64_TRUNC_SAT_F64_U)
     ; ("i64x2.abs", I64X2_ABS)
     ; ("i64x2.add", I64X2_ADD)
     ; ("i64x2.eq", I64X2_EQ)
     ; ("i64x2.extmul_low_i32x4_s", I64X2_EXTMUL_LOW_I32X4_S)
     ; ("i64x2", I64X2)
     ; ("i64x2.mul", I64X2_MUL)
     ; ("i64x2.sub", I64X2_SUB)
     ; ("i64.xor", I64_XOR)
     ; ("i8", I8)
     ; ("i8x16.add", I8X16_ADD)
     ; ("i8x16.add_sat_s", I8X16_ADD_SAT_S)
     ; ("i8x16_add_sat_s", I8X16_ADD_SAT_S)
     ; ("i8x16.eq", I8X16_EQ)
     ; ("i8x16.extract_lane_s", I8X16_EXTRACT_LANE_S)
     ; ("i8x16", I8X16)
     ; ("i8x16.min_s", I8X16_MIN_S)
     ; ("i8x16.shl", I8X16_SHL)
     ; ("i8x16.splat", I8X16_SPLAT)
     ; ("if", IF)
     ; ("import", IMPORT)
     ; ("instance", INSTANCE)
     ; ("invoke", INVOKE)
     ; ("item", ITEM)
     ; ("local.get", LOCAL_GET)
     ; ("local", LOCAL)
     ; ("local.set", LOCAL_SET)
     ; ("local.tee", LOCAL_TEE)
     ; ("loop", LOOP)
     ; ("memory.copy", MEMORY_COPY)
     ; ("memory.fill", MEMORY_FILL)
     ; ("memory.grow", MEMORY_GROW)
     ; ("memory.init", MEMORY_INIT)
     ; ("memory", MEMORY)
     ; ("memory.size", MEMORY_SIZE)
     ; ("module", MODULE)
     ; ("mut", MUTABLE)
     ; ("nan:arithmetic", NAN_ARITH)
     ; ("nan:canonical", NAN_CANON)
     ; ("noexn", NO_EXN)
     ; ("noextern", NOEXTERN)
     ; ("nofunc", NOFUNC)
     ; ("none", NONE)
     ; ("nop", NOP)
     ; ("nullexnref", NULL_EXN_REF)
     ; ("nullexternref", NULL_EXTERN_REF)
     ; ("nullfuncref", NULL_FUNC_REF)
     ; ("null", NULL)
     ; ("nullref", NULL_REF)
     ; ("offset", OFFSET)
     ; ("param", PARAM)
     ; ("quote", QUOTE)
     ; ("rec", REC)
     ; ("ref.array", REF_ARRAY)
     ; ("ref.as_non_null", REF_AS_NON_NULL)
     ; ("ref.cast", REF_CAST)
     ; ("ref.eq", REF_EQ)
     ; ("ref.extern", REF_EXTERN)
     ; ("ref.func", REF_FUNC)
     ; ("ref.host", REF_HOST)
     ; ("ref.i31", REF_I31)
     ; ("ref.is_null", REF_IS_NULL)
     ; ("ref.null", REF_NULL)
     ; ("ref", REF)
     ; ("ref.struct", REF_STRUCT)
     ; ("ref.test", REF_TEST)
     ; ("register", REGISTER)
     ; ("result", RESULT)
     ; ("return_call_indirect", RETURN_CALL_INDIRECT)
     ; ("return_call_ref", RETURN_CALL_REF)
     ; ("return_call", RETURN_CALL)
     ; ("return", RETURN)
     ; ("select", SELECT)
     ; ("start", START)
     ; ("struct.get_s", STRUCT_GET_S)
     ; ("struct.get", STRUCT_GET)
     ; ("struct.get_u", STRUCT_GET_U)
     ; ("struct.new_default", STRUCT_NEW_CANON_DEFAULT)
     ; ("struct.new", STRUCT_NEW_CANON)
     ; ("structref", STRUCT_REF)
     ; ("struct.set", STRUCT_SET)
     ; ("struct", STRUCT)
     ; ("sub", SUB)
     ; ("table.copy", TABLE_COPY)
     ; ("table.fill", TABLE_FILL)
     ; ("table.get", TABLE_GET)
     ; ("table.grow", TABLE_GROW)
     ; ("table.init", TABLE_INIT)
     ; ("table.set", TABLE_SET)
     ; ("table.size", TABLE_SIZE)
     ; ("table", TABLE)
     ; ("tag", TAG)
     ; ("then", THEN)
     ; ("type", TYPE)
     ; ("unreachable", UNREACHABLE)
     ; ("v128.any_true", V128_ANY_TRUE)
     ; ("v128.const", V128_CONST)
     ; ("v128.load16_lane", V128_LOAD16_LANE)
     ; ("v128.load32_lane", V128_LOAD32_LANE)
     ; ("v128.load32_zero", V128_LOAD32_ZERO)
     ; ("v128.load64_lane", V128_LOAD64_LANE)
     ; ("v128.load8_lane", V128_LOAD8_LANE)
     ; ("v128.load8_splat", V128_LOAD8_SPLAT)
     ; ("v128.load8x8_s", V128_LOAD8X8_S)
     ; ("v128.load", V128_LOAD)
     ; ("v128.not", V128_NOT)
     ; ("v128.store16_lane", V128_STORE16_LANE)
     ; ("v128.store32_lane", V128_STORE32_LANE)
     ; ("v128.store32_zero", V128_STORE32_ZERO)
     ; ("v128.store64_lane", V128_STORE64_LANE)
     ; ("v128.store8_lane", V128_STORE8_LANE)
     ; ("v128.store", V128_STORE)
     ; ("v128", V128)
     ; ("v128.load8x8_u", V128_LOAD8X8_U)
     ; ("i8x16.shr_s", I8X16_SHR_S)
     ; ("v128.and", V128_AND)
     ; ("i8x16.all_true", I8X16_ALL_TRUE)
     ; ("f32x4.convert_i32x4_u", F32X2_CONVERT_I32X4_U)
     ; ("f32x4.sub", F32X4_SUB)
     ; ("f32x4.ne", F32X4_NE)
     ; ("f32x4.pmax", F32X4_PMAX)
     ; ("f32x4.floor", F32X4_FLOOR)
     ; ("f32x4.max", F32X4_MAX)
     ; ("f64x2.sub", F64X2_SUB)
     ; ("f64x2.ne", F64X2_NE)
     ; ("f64x2.pmax", F64X2_PMAX)
     ; ("f64x2.floor", F64X2_FLOOR)
     ; ("f64x2.max", F64X2_MAX)
     ; ("i16x8.min_u", I16X8_MIN_U)
     ; ("i16x8.sub", I16X8_SUB)
     ; ("i16x8.ne", I16X8_NE)
     ; ("i16x8.extadd_pairwise_i8x16_u", I16X8_EXTADD_PAIRWISE_I8X16_U)
     ; ("i16x8.extmul_high_i8x16_s", I16X8_EXTMUL_HIGH_I8X16_S)
     ; ("i16x8.add_sat_u", I16X8_ADD_SAT_U)
     ; ("i32x4.min_u", I32X4_MIN_U)
     ; ("i32x4.neg", I32X4_NEG)
     ; ("i32x4.ne", I32X4_NE)
     ; ("i32x4.extadd_pairwise_i16x8_u", I32X4_EXTADD_PAIRWISE_I16X8_U)
     ; ("i32x4.extmul_high_i16x8_s", I32X4_EXTMUL_HIGH_I16X8_S)
     ; ("i32x4.trunc_sat_f32x4_u", I32X4_TRUNC_SAT_F32X4_U)
     ; ("i32x4.trunc_sat_f64x2_u_zero", I32X4_TRUNC_SAT_F64X2_U_ZERO)
     ; ("i64x2.neg", I64X2_NEG)
     ; ("i64x2.ne", I64X2_NE)
     ; ("i64x2.extmul_high_i32x4_s", I64X2_EXTMUL_HIGH_I32X4_S)
     ; ("i8x16.min_u", I8X16_MIN_U)
     ; ("i8x16.sub", I8X16_SUB)
     ; ("i8x16.ne", I8X16_NE)
     ; ("i8x16.add_sat_u", I8X16_ADD_SAT_U)
     ; ("i16x8.extend_high_i8x16_u", I16X8_EXTEND_HIGH_I8X16_U)
     ; ("i8x16.extract_lane_u", I8X16_EXTRACT_LANE_U)
     ; ("v128.load16_splat", V128_LOAD16_SPLAT)
     ; ("i8x16.all_true", I8X16_ALL_TRUE)
     ; ("v128.load64_zero", V128_LOAD64_ZERO)
     ; ("i16x8.splat", I16X8_SPLAT)
     ; ("v128.load16x4_s", V128_LOAD16X4_S)
     ; ("i8x16.shr_u", I8X16_SHR_U)
     ; ("v128.or", V128_OR)
     ; ("i8x16.bitmask", I8X16_BITMASK)
     ; ("f64x2.convert_low_i32x4_s", F64X2_CONVERT_LOW_I32X4_S)
     ; ("f32x4.mul", F32X4_MUL)
     ; ("f32x4.lt", F32X4_LT)
     ; ("f32x4.trunc", F32X4_TRUNC)
     ; ("f32x4.abs", F32X4_ABS)
     ; ("f64x2.mul", F64X2_MUL)
     ; ("f64x2.lt", F64X2_LT)
     ; ("f64x2.trunc", F64X2_TRUNC)
     ; ("f64x2.abs", F64X2_ABS)
     ; ("i16x8.max_s", I16X8_MAX_S)
     ; ("i16x8.mul", I16X8_MUL)
     ; ("i16x8.lt_s", I16X8_LT_S)
     ; ("i16x8.extmul_low_i8x16_u", I16X8_EXTMUL_LOW_I8X16_U)
     ; ("i16x8.sub_sat_s", I16X8_SUB_SAT_S)
     ; ("i32x4.max_s", I32X4_MAX_S)
     ; ("i32x4.lt_s", I32X4_LT_S)
     ; ("i32x4.extmul_low_i16x8_u", I32X4_EXTMUL_LOW_I16X8_U)
     ; ("i64x2.lt_s", I64X2_LT_S)
     ; ("i64x2.extmul_low_i32x4_u", I64X2_EXTMUL_LOW_I32X4_U)
     ; ("i8x16.max_s", I8X16_MAX_S)
     ; ("i8x16.neg", I8X16_NEG)
     ; ("i8x16.lt_s", I8X16_LT_S)
     ; ("i8x16.sub_sat_s", I8X16_SUB_SAT_S)
     ; ("i16x8.extend_low_i8x16_s", I16X8_EXTEND_LOW_I8X16_S)
     ; ("i16x8.extract_lane_s", I16X8_EXTRACT_LANE_S)
     ; ("v128.load16x4_s", V128_LOAD16X4_S)
     ; ("v128.load32_splat", V128_LOAD32_SPLAT)
     ; ("v128.bitselect", V128_BITSELECT)
     ; ("i32x4.extract_lane", I32X4_EXTRACT_LANE)
     ; ("i32x4.splat", I32X4_SPLAT)
     ; ("f32x4.div", F32X4_DIV)
     ; ("f32x4.le", F32X4_LE)
     ; ("f32x4.nearest", F32X4_NEAREST)
     ; ("f32x4.splat", F32X4_SPLAT)
     ; ("f64x2.convert_low_i32x4_u", F64X2_CONVERT_LOW_I32X4_U)
     ; ("f64x2.div", F64X2_DIV)
     ; ("f64x2.le", F64X2_LE)
     ; ("f64x2.nearest", F64X2_NEAREST)
     ; ("i16x8.all_true", I16X8_ALL_TRUE)
     ; ("i16x8.extend_low_i8x16_u", I16X8_EXTEND_LOW_I8X16_U)
     ; ("i16x8.extmul_high_i8x16_u", I16X8_EXTMUL_HIGH_I8X16_U)
     ; ("i16x8.extract_lane_u", I16X8_EXTRACT_LANE_U)
     ; ("i16x8.lt_u", I16X8_LT_U)
     ; ("i16x8.max_u", I16X8_MAX_U)
     ; ("i16x8.neg", I16X8_NEG)
     ; ("i16x8.shl", I16X8_SHL)
     ; ("i16x8.sub_sat_u", I16X8_SUB_SAT_U)
     ; ("i32x4.extmul_high_i16x8_u", I32X4_EXTMUL_HIGH_I16X8_U)
     ; ("i32x4.lt_u", I32X4_LT_U)
     ; ("i32x4.max_u", I32X4_MAX_U)
     ; ("i64x2.extmul_high_i32x4_u", I64X2_EXTMUL_HIGH_I32X4_U)
     ; ("i64x2.extract_lane", I64X2_EXTRACT_LANE)
     ; ("i64x2.le_s", I64X2_LE_S)
     ; ("i8x16.lt_u", I8X16_LT_U)
     ; ("i8x16.max_u", I8X16_MAX_U)
     ; ("i8x16.sub_sat_u", I8X16_SUB_SAT_U)
     ; ("i8x16.swizzle", I8X16_SWIZZLE)
     ; ("v128.load16x4_u", V128_LOAD16X4_U)
     ; ("v128.load64_splat", V128_LOAD64_SPLAT)
     ; ("v128.xor", V128_XOR)
     ; ("f32x4.extract_lane", F32X4_EXTRACT_LANE)
     ; ("f32x4.gt", F32X4_GT)
     ; ("f32x4.neg", F32X4_NEG)
     ; ("f64x2.gt", F64X2_GT)
     ; ("f64x2.neg", F64X2_NEG)
     ; ("i16x8.avgr_u", I16X8_AVGR_U)
     ; ("i16x8.bitmask", I16X8_BITMASK)
     ; ("i16x8.le_s", I16X8_LE_S)
     ; ("i16x8.shr_s", I16X8_SHR_S)
     ; ("i32x4.abs", I32X4_ABS)
     ; ("i32x4.extend_high_i16x8_s", I32X4_EXTEND_HIGH_I16X8_S)
     ; ("i32x4.le_s", I32X4_LE_S)
     ; ("i64x2.gt_s", I64X2_GT_S)
     ; ("i64x2.splat", I64X2_SPLAT)
     ; ("i8x16.avgr_u", I8X16_AVGR_U)
     ; ("i8x16.le_s", I8X16_LE_S)
     ; ("i8x16.narrow_i16x8_s", I8X16_NARROW_I16X8_S)
     ; ("v128.andnot", V128_ANDNOT)
     ; ("v128.load32x2_s", V128_LOAD32X2_S)
    |];
  tbl

let rec token buf =
  match%sedlex buf with
  | Plus any_blank -> token buf
  | bad_num | bad_id | bad_name -> unknown_operator buf
  | num -> NUM (Utf8.lexeme buf)
  | operator -> begin
    let operator = Utf8.lexeme buf in
    match Hashtbl.find_opt keywords operator with
    | None -> unknown_operator buf
    | Some v -> v
    end
  (* comment *)
  | ";;" ->
    single_comment buf;
    token buf
  | "(;" ->
    comment buf;
    token buf
  (* custom annotation *)
  | "(@", name ->
    let annotid = Utf8.lexeme buf in
    let annotid = String.sub annotid 3 (String.length annotid - 4) in
    let annotid = mk_string buf annotid in
    if String.equal "" annotid then raise Empty_annotation_id
    else begin
      annot buf;
      token buf
    end
  | "(@", Plus id_char ->
    annot buf;
    token buf
  | "(@" -> raise Empty_annotation_id
  (* 1 *)
  | "(" -> LPAR
  | ")" -> RPAR
  | "=" -> EQUAL
  (* other *)
  | id ->
    let id = Utf8.lexeme buf in
    let id = String.sub id 1 (String.length id - 1) in
    ID id
  | "$" -> raise Empty_identifier
  | name ->
    let name = Utf8.lexeme buf in
    let name = String.sub name 1 (String.length name - 2) in
    let name = mk_string buf name in
    NAME name
  | "\"", Star string_elem -> raise Unclosed_string
  | eof -> EOF
  (* | "" -> EOF *)
  | any -> unknown_operator buf
  | _ -> unknown_operator buf

and comment buf =
  match%sedlex buf with
  | ";)" -> ()
  | "(;" ->
    comment buf;
    comment buf
  | eof -> raise Unclosed_comment
  | any -> comment buf
  | _ -> assert false

and single_comment buf =
  match%sedlex buf with
  | newline -> ()
  | eof -> raise Unclosed_comment
  | any -> single_comment buf
  | _ -> assert false

and annot buf =
  match%sedlex buf with
  | Plus any_blank -> annot buf
  | ";;" ->
    single_comment buf;
    annot buf
  | "(;" ->
    comment buf;
    annot buf
  | "(" ->
    annot buf;
    annot buf
  | ")" -> ()
  | "\"", Star string_elem -> raise Unclosed_string
  | eof -> raise Unclosed_annotation
  | annot_atom -> annot buf
  | _ -> illegal_character buf

let lexer buf = Sedlexing.with_tokenizer token buf
