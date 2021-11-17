open Sedlexing

type token = Menhir_parser.token

open Menhir_parser

exception LexError of Lexing.position * string

let blank = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let any_blank = [%sedlex.regexp? blank | newline]

let sign = [%sedlex.regexp? '+' | '-']

let digit = [%sedlex.regexp? '0' .. '9']

let hexdigit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let num = [%sedlex.regexp? digit, Star (Opt '_', digit)]

let hexnum = [%sedlex.regexp? hexdigit, Star (Opt '_' | hexdigit)]

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
    Plus
      ( '0' .. '9'
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/' | ':'
      | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~' | '*'
        )]

let name = [%sedlex.regexp? "\"", Star (Sub (any, "\"") | "\\\""), "\""]

let id = [%sedlex.regexp? "$", Plus id_char]

let rec token buf =
  match%sedlex buf with
  | Plus any_blank -> token buf
  (* 16 *)
  | "assert_malformed" -> ASSERT_MALFORMED
  (* 14 *)
  | "assert_invalid" -> ASSERT_INVALID
  (* 13 *)
  | "assert_return" -> ASSERT_RETURN
  | "call_indirect" -> CALL_INDIRECT
  (* 11 *)
  | "assert_trap" -> ASSERT_TRAP
  | "promote_f32" -> PROMOTE_F32
  | "reinterpret" -> REINTERPRET
  | "unreachable" -> UNREACHABLE
  (* 10 *)
  | "demote_f64" -> DEMOTE_F64
  | "extend_i32" -> EXTEND_I32
  (* 9 *)
  | "externref" -> EXTERNREF
  | "trunc_sat" -> TRUNC_SAT
  (* 8 *)
  | "br_table" -> BR_TABLE
  | "copysign" -> COPYSIGN
  | "extend16" -> EXTEND16
  | "extend32" -> EXTEND32
  | "register" -> REGISTER
  | "wrap_i64" -> WRAP_I64
  (* 7 *)
  | "convert" -> CONVERT
  | "declare" -> DECLARE
  | "extend8" -> EXTEND8
  | "funcref" -> FUNCREF
  | "is_null" -> IS_NULL
  | "nearest" -> NEAREST
  | "store16" -> STORE16
  | "store32" -> STORE32
  (* 6 *)
  | "binary" -> BINARY
  | "export" -> EXPORT
  | "extern" -> EXTERN
  | "global" -> GLOBAL
  | "import" -> IMPORT
  | "invoke" -> INVOKE
  | "load16" -> LOAD16
  | "load32" -> LOAD32
  | "memory" -> MEMORY
  | "module" -> MODULE
  | "offset" -> OFFSET
  | "popcnt" -> POPCNT
  | "result" -> RESULT
  | "return" -> RETURN
  | "select" -> SELECT
  | "store8" -> STORE8
  (* 5 *)
  | "align" -> ALIGN
  | "block" -> BLOCK
  | "br_if" -> BR_IF
  | "const" -> CONST
  | "floor" -> FLOOR
  | "load8" -> LOAD8
  | "local" -> LOCAL
  | "param" -> PARAM
  | "quote" -> QUOTE
  | "start" -> START
  | "store" -> STORE
  | "table" -> TABLE
  | "trunc" -> TRUNC
  (* 4 *)
  | "call" -> CALL
  | "ceil" -> CEIL
  | "copy" -> COPY
  | "data" -> DATA
  | "drop" -> DROP
  | "elem" -> ELEM
  | "else" -> ELSE
  | "fill" -> FILL
  | "func" -> FUNC
  | "grow" -> GROW
  | "init" -> INIT
  | "item" -> ITEM
  | "load" -> LOAD
  | "loop" -> LOOP
  | "null" -> NULL
  | "rotl" -> ROTL
  | "rotr" -> ROTR
  | "size" -> SIZE
  | "sqrt" -> SQRT
  | "then" -> THEN
  | "type" -> TYPE
  (* 3 *)
  | "abs" -> ABS
  | "add" -> ADD
  | "and" -> AND
  | "clz" -> CLZ
  | "ctz" -> CTZ
  | "div" -> DIV
  | "eqz" -> EQZ
  | "end" -> END
  | "f32" -> F32
  | "f64" -> F64
  | "get" -> GET
  | "i32" -> I32
  | "i64" -> I64
  | "max" -> MAX
  | "min" -> MIN
  | "mul" -> MUL
  | "mut" -> MUTABLE
  | "neg" -> NEG
  | "nop" -> NOP
  | "ref" -> REF
  | "rem" -> REM
  | "set" -> SET
  | "shl" -> SHL
  | "shr" -> SHR
  | "sub" -> SUB
  | "tee" -> TEE
  | "xor" -> XOR
  (* 2 *)
  | "br" -> BR
  | "eq" -> EQ
  | "ge" -> GE
  | "gt" -> GT
  | "if" -> IF
  | "le" -> LE
  | "lt" -> LT
  | "ne" -> NE
  | "or" -> OR
  | "_s" -> SIGNED
  | "_u" -> UNSIGNED
  | ";;" ->
    single_comment buf;
    token buf
  | "(;" ->
    comment buf;
    token buf
  (* 1 *)
  | "(" -> LPAR
  | ")" -> RPAR
  | "." -> DOT
  | "_" -> UNDERSCORE
  | "=" -> EQUAL
  (* other *)
  | id ->
    let id = Utf8.lexeme buf in
    let id = String.sub id 1 (String.length id - 1) in
    ID id
  | num -> NUM (Utf8.lexeme buf)
  | name -> NAME (Utf8.lexeme buf)
  | eof -> EOF
  (* | "" -> EOF *)
  | _ ->
    let position = fst @@ lexing_positions buf in
    let tok = Utf8.lexeme buf in
    raise @@ LexError (position, Printf.sprintf "unexpected character `%S`" tok)

and comment buf =
  match%sedlex buf with
  | ";)" -> ()
  | "(;" ->
    comment buf;
    comment buf
  | eof -> failwith "eof in comment"
  | any -> comment buf
  | _ -> assert false

and single_comment buf =
  match%sedlex buf with
  | newline -> ()
  | eof -> failwith "eof in single line comment"
  | any -> single_comment buf
  | _ -> assert false

let lexer buf = Sedlexing.with_tokenizer token buf
