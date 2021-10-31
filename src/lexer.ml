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

let rec nom buf =
  match%sedlex buf with
  | Plus any_blank -> nom buf
  | _ -> ()

let token buf =
  nom buf;
  match%sedlex buf with
  | "module" -> MODULE
  | "export" -> EXPORT
  | "result" -> RESULT
  | "param" -> PARAM
  | "const" -> CONST
  | "func" -> FUNC
  | "i32" -> I32
  | "i64" -> I64
  | "f32" -> F32
  | "f64" -> F64
  | "if" -> IF
  | "(" -> LPAR
  | ")" -> RPAR
  | "." -> DOT
  | num -> INT (Int64.of_string (Utf8.lexeme buf))
  | name -> NAME (Utf8.lexeme buf)
  | eof -> EOF
  | id -> ID (Utf8.lexeme buf)
  | "" -> EOF
  | _ ->
    let position = fst @@ lexing_positions buf in
    let tok = Utf8.lexeme buf in
    raise @@ LexError (position, Printf.sprintf "unexpected character %S" tok)

let lexer buf = Sedlexing.with_tokenizer token buf
