(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module for Wasm lexing. *)

(** lexing error exception *)
exception Illegal_escape of string

exception Unknown_operator of string

exception Unexpected_character of string

(** tokenizer *)
val token : Sedlexing.lexbuf -> Text_parser.token

(** lexer *)
val lexer :
     Sedlexing.lexbuf
  -> unit
  -> Text_parser.token * Lexing.position * Lexing.position
