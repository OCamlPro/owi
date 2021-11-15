open Sedlexing

type token = Menhir_parser.token

open Menhir_parser

exception LexError of Lexing.position * string

let blank = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let any_blank = [%sedlex.regexp? blank | newline]

let num = [%sedlex.regexp? Opt '-', Opt ('0', 'x'), Plus '0' .. '9', Opt ('.', Plus '0' .. '9')]

let id_char =
  [%sedlex.regexp?
    Plus
      ( '0' .. '9'
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/' | ':'
      | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~' | '*'
        )]

let name = [%sedlex.regexp? "\"", Star (Sub (any, "\"")), "\""]

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
  | "unreachable" -> UNREACHABLE
  (* 10 *)
  | "extend_i32" -> EXTEND_I32
  (* 8 *)
  | "br_table" -> BR_TABLE
  (* 7 *)
  | "funcref" -> FUNCREF
  (* 6 *)
  | "export" -> EXPORT
  | "global" -> GLOBAL
  | "import" -> IMPORT
  | "invoke" -> INVOKE
  | "memory" -> MEMORY
  | "module" -> MODULE
  | "result" -> RESULT
  | "return" -> RETURN
  | "select" -> SELECT
  (* 5 *)
  | "block" -> BLOCK
  | "br_if" -> BR_IF
  | "const" -> CONST
  | "local" -> LOCAL
  | "param" -> PARAM
  | "quote" -> QUOTE
  | "store" -> STORE
  | "table" -> TABLE
  (* 4 *)
  | "call" -> CALL
  | "drop" -> DROP
  | "elem" -> ELEM
  | "else" -> ELSE
  | "func" -> FUNC
  | "grow" -> GROW
  | "load" -> LOAD
  | "loop" -> LOOP
  | "then" -> THEN
  | "type" -> TYPE
  (* 3 *)
  | "add" -> ADD
  | "ctz" -> CTZ
  | "eqz" -> EQZ
  | "f32" -> F32
  | "f64" -> F64
  | "get" -> GET
  | "i32" -> I32
  | "i64" -> I64
  | "mul" -> MUL
  | "mut" -> MUTABLE
  | "nop" -> NOP
  | "set" -> SET
  | "sub" -> SUB
  | "tee" -> TEE
  (* 2 *)
  | "br" -> BR
  | "eq" -> EQ
  | "gt" -> GT
  | "if" -> IF
  | "lt" -> LT
  | "_s" -> SIGNED
  | "_u" -> UNSIGNED
  | ";;" -> single_comment buf; token buf
  | "(;" -> comment buf; token buf
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
  | "(;" -> comment buf; comment buf
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
