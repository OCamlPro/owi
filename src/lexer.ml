open Sedlexing

type token = Menhir_parser.token

open Menhir_parser

exception LexError of Lexing.position * string

let blank = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let any_blank = [%sedlex.regexp? blank | newline]

let num = [%sedlex.regexp? Plus '0' .. '9']

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
  | "assert_return" -> ASSERT_RETURN
  | "unreachable" -> UNREACHABLE
  | "module" -> MODULE
  | "invoke" -> INVOKE
  | "export" -> EXPORT
  | "import" -> IMPORT
  | "result" -> RESULT
  | "return" -> RETURN
  | "param" -> PARAM
  | "local" -> LOCAL
  | "const" -> CONST
  | "else" -> ELSE
  | "func" -> FUNC
  | "then" -> THEN
  | "get" -> GET
  | "nop" -> NOP
  | "i32" -> I32
  | "i64" -> I64
  | "f32" -> F32
  | "f64" -> F64
  | "if" -> IF
  | "_s" -> SIGNED
  | "_u" -> UNSIGNED
  | "lt" -> LT
  | "eq" -> EQ
  | "(;" -> comment buf; token buf
  | "(" -> LPAR
  | ")" -> RPAR
  | "." -> DOT
  | "_" -> UNDERSCORE
  | "=" -> EQUAL
  | num -> NUM (Utf8.lexeme buf)
  | name -> NAME (Utf8.lexeme buf)
  | eof -> EOF
  | id ->
    let id = Utf8.lexeme buf in
    let id = String.sub id 1 (String.length id - 1) in
    ID id
  (* | "" -> EOF *)
  | _ ->
    let position = fst @@ lexing_positions buf in
    let tok = Utf8.lexeme buf in
    raise @@ LexError (position, Printf.sprintf "unexpected character %S" tok)
and comment buf =
  match%sedlex buf with
  | ";)" -> ()
  | "(;" -> comment buf; comment buf
  | eof -> failwith "eof in comment"
  | any -> comment buf
  | _ -> assert false

let lexer buf = Sedlexing.with_tokenizer token buf
