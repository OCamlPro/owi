(** Module for Wasm lexing. *)

(** lexing error exception *)
exception Illegal_escape of string
exception Unknown_operator of string
exception Unexpected_character of string

(** tokenizer *)
val token : Sedlexing.lexbuf -> Menhir_parser.token

(** lexer *)
val lexer :
     Sedlexing.lexbuf
  -> unit
  -> Menhir_parser.token * Lexing.position * Lexing.position
