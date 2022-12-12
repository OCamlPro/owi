(** Module for Wasm lexing. *)

(** lexing error exception *)
exception Error of Lexing.position * string

(** tokenizer *)
val token : Sedlexing.lexbuf -> Menhir_parser.token

(** lexer *)
val lexer :
     Sedlexing.lexbuf
  -> unit
  -> Menhir_parser.token * Lexing.position * Lexing.position
