(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
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
     ; ("anyref", ANY_REF)
     ; ("array", ARRAY)
     ; ("array.get", ARRAY_GET)
     ; ("array.get_u", ARRAY_GET_U)
     ; ("array.len", ARRAY_LEN)
     ; ("array.new", ARRAY_NEW_CANON)
     ; ("array.new_data", ARRAY_NEW_CANON_DATA)
     ; ("array.new_default", ARRAY_NEW_CANON_DEFAULT)
     ; ("array.new_elem", ARRAY_NEW_CANON_ELEM)
     ; ("array.new_fixed", ARRAY_NEW_CANON_FIXED)
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
     ; ("br_on_non_null", BR_ON_NON_NULL)
     ; ("br_on_null", BR_ON_NULL)
     ; ("br_on_cast", BR_ON_CAST)
     ; ("br_on_cast_fail", BR_ON_CAST_FAIL)
     ; ("br_if", BR_IF)
     ; ("br_table", BR_TABLE)
     ; ("call", CALL)
     ; ("call_indirect", CALL_INDIRECT)
     ; ("call_ref", CALL_REF)
     ; ("data", DATA)
     ; ("data.drop", DATA_DROP)
     ; ("declare", DECLARE)
     ; ("drop", DROP)
     ; ("elem", ELEM)
     ; ("elem.drop", ELEM_DROP)
     ; ("else", ELSE)
     ; ("end", END)
     ; ("eq", EQ)
     ; ("export", EXPORT)
     ; ("extern", EXTERN)
     ; ("externref", EXTERN_REF)
     ; ("extern.externalize", EXTERN_EXTERNALIZE)
     ; ("extern.internalize", EXTERN_INTERNALIZE)
     ; ("f32", F32)
     ; ("f32x4", F32X4)
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
     ; ("f32.floor", F32_FLOOR)
     ; ("f32.ge", F32_GE)
     ; ("f32.gt", F32_GT)
     ; ("f32.le", F32_LE)
     ; ("f32.load", F32_LOAD)
     ; ("f32.lt", F32_LT)
     ; ("f32.max", F32_MAX)
     ; ("f32.min", F32_MIN)
     ; ("f32.mul", F32_MUL)
     ; ("f32.ne", F32_NE)
     ; ("f32.nearest", F32_NEAREST)
     ; ("f32.neg", F32_NEG)
     ; ("f32.reinterpret_i32", F32_REINTERPRET_I32)
     ; ("f32.reinterpret_i64", F32_REINTERPRET_I64)
     ; ("f32.sqrt", F32_SQRT)
     ; ("f32.store", F32_STORE)
     ; ("f32.sub", F32_SUB)
     ; ("f32.trunc", F32_TRUNC)
     ; ("f64", F64)
     ; ("f64x2", F64X2)
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
     ; ("f64.floor", F64_FLOOR)
     ; ("f64.ge", F64_GE)
     ; ("f64.gt", F64_GT)
     ; ("f64.le", F64_LE)
     ; ("f64.load", F64_LOAD)
     ; ("f64.lt", F64_LT)
     ; ("f64.max", F64_MAX)
     ; ("f64.min", F64_MIN)
     ; ("f64.mul", F64_MUL)
     ; ("f64.ne", F64_NE)
     ; ("f64.nearest", F64_NEAREST)
     ; ("f64.neg", F64_NEG)
     ; ("f64.promote_f32", F64_PROMOTE_F32)
     ; ("f64.reinterpret_i32", F64_REINTERPRET_I32)
     ; ("f64.reinterpret_i64", F64_REINTERPRET_I64)
     ; ("f64.sqrt", F64_SQRT)
     ; ("f64.store", F64_STORE)
     ; ("f64.sub", F64_SUB)
     ; ("f64.trunc", F64_TRUNC)
     ; ("field", FIELD)
     ; ("final", FINAL)
     ; ("func", FUNC)
     ; ("funcref", FUNC_REF)
     ; ("get", GET)
     ; ("global", GLOBAL)
     ; ("global.get", GLOBAL_GET)
     ; ("global.set", GLOBAL_SET)
     ; ("i16", I16)
     ; ("i16x8", I16X8)
     ; ("i31", I31)
     ; ("i31.get_s", I31_GET_S)
     ; ("i31.get_u", I31_GET_U)
     ; ("i32", I32)
     ; ("i32x4", I32X4)
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
     ; ("i32.le_s", I32_LE_S)
     ; ("i32.le_u", I32_LE_U)
     ; ("i32.load", I32_LOAD)
     ; ("i32.load16_s", I32_LOAD16_S)
     ; ("i32.load16_u", I32_LOAD16_U)
     ; ("i32.load8_s", I32_LOAD8_S)
     ; ("i32.load8_u", I32_LOAD8_U)
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
     ; ("i32.store", I32_STORE)
     ; ("i32.store16", I32_STORE16)
     ; ("i32.store8", I32_STORE8)
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
     ; ("i32.xor", I32_XOR)
     ; ("i64", I64)
     ; ("i64x2", I64X2)
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
     ; ("i64.le_s", I64_LE_S)
     ; ("i64.le_u", I64_LE_U)
     ; ("i64.load", I64_LOAD)
     ; ("i64.load16_s", I64_LOAD16_S)
     ; ("i64.load16_u", I64_LOAD16_U)
     ; ("i64.load32_s", I64_LOAD32_S)
     ; ("i64.load32_u", I64_LOAD32_U)
     ; ("i64.load8_s", I64_LOAD8_S)
     ; ("i64.load8_u", I64_LOAD8_U)
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
     ; ("i64.store", I64_STORE)
     ; ("i64.store16", I64_STORE16)
     ; ("i64.store32", I64_STORE32)
     ; ("i64.store8", I64_STORE8)
     ; ("i64.sub", I64_SUB)
     ; ("i64.trunc_f32_s", I64_TRUNC_F32_S)
     ; ("i64.trunc_f32_u", I64_TRUNC_F32_U)
     ; ("i64.trunc_f64_s", I64_TRUNC_F64_S)
     ; ("i64.trunc_f64_u", I64_TRUNC_F64_U)
     ; ("i64.trunc_sat_f32_s", I64_TRUNC_SAT_F32_S)
     ; ("i64.trunc_sat_f32_u", I64_TRUNC_SAT_F32_U)
     ; ("i64.trunc_sat_f64_s", I64_TRUNC_SAT_F64_S)
     ; ("i64.trunc_sat_f64_u", I64_TRUNC_SAT_F64_U)
     ; ("i64.xor", I64_XOR)
     ; ("i8", I8)
     ; ("i8x16", I8X16)
     ; ("if", IF)
     ; ("import", IMPORT)
     ; ("invoke", INVOKE)
     ; ("item", ITEM)
     ; ("local", LOCAL)
     ; ("local.get", LOCAL_GET)
     ; ("local.set", LOCAL_SET)
     ; ("local.tee", LOCAL_TEE)
     ; ("loop", LOOP)
     ; ("memory", MEMORY)
     ; ("memory.copy", MEMORY_COPY)
     ; ("memory.fill", MEMORY_FILL)
     ; ("memory.grow", MEMORY_GROW)
     ; ("memory.init", MEMORY_INIT)
     ; ("memory.size", MEMORY_SIZE)
     ; ("module", MODULE)
     ; ("definition", DEFINITION)
     ; ("instance", INSTANCE)
     ; ("mut", MUTABLE)
     ; ("nan:arithmetic", NAN_ARITH)
     ; ("nan:canonical", NAN_CANON)
     ; ("noextern", NOEXTERN)
     ; ("nofunc", NOFUNC)
     ; ("none", NONE)
     ; ("null", NULL)
     ; ("nop", NOP)
     ; ("offset", OFFSET)
     ; ("param", PARAM)
     ; ("quote", QUOTE)
     ; ("rec", REC)
     ; ("ref", REF)
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
     ; ("ref.struct", REF_STRUCT)
     ; ("ref.test", REF_TEST)
     ; ("register", REGISTER)
     ; ("result", RESULT)
     ; ("return", RETURN)
     ; ("return_call", RETURN_CALL)
     ; ("return_call_indirect", RETURN_CALL_INDIRECT)
     ; ("return_call_ref", RETURN_CALL_REF)
     ; ("select", SELECT)
     ; ("start", START)
     ; ("struct", STRUCT)
     ; ("structref", STRUCTREF)
     ; ("struct.get", STRUCT_GET)
     ; ("struct.get_s", STRUCT_GET_S)
     ; ("struct.new", STRUCT_NEW_CANON)
     ; ("struct.new_default", STRUCT_NEW_CANON_DEFAULT)
     ; ("struct.set", STRUCT_SET)
     ; ("sub", SUB)
     ; ("table", TABLE)
     ; ("table.copy", TABLE_COPY)
     ; ("table.fill", TABLE_FILL)
     ; ("table.get", TABLE_GET)
     ; ("table.grow", TABLE_GROW)
     ; ("table.init", TABLE_INIT)
     ; ("table.set", TABLE_SET)
     ; ("table.size", TABLE_SIZE)
     ; ("then", THEN)
     ; ("type", TYPE)
     ; ("unreachable", UNREACHABLE)
     ; ("v128", V128)
     ; ("v128.const", V128_CONST)
     ; ("i32x4.add", I32X4_ADD)
     ; ("i32x4.sub", I32X4_SUB)
     ; ("i64x2.add", I64X2_ADD)
     ; ("i64x2.sub", I64X2_SUB)
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
