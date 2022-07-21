type token = Menhir_parser.token

exception Error of Lexing.position * string

val token : Sedlexing.lexbuf -> token

val lexer :
  Sedlexing.lexbuf -> unit -> token * Lexing.position * Lexing.position
