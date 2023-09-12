(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

module Make (M : sig
  type t

  val rule : (Lexing.lexbuf -> Menhir_parser.token) -> Lexing.lexbuf -> t
end) =
struct
  let rule = Obj.magic M.rule

  let from_lexbuf =
    let parser = MenhirLib.Convert.Simplified.traditional2revised rule in
    let print_err buf msg =
      let pos = fst @@ Sedlexing.lexing_positions buf in
      let file_line =
        let cpos = pos.pos_cnum - pos.pos_bol in
        Printf.sprintf "File \"%s\", line %i, character %i:" pos.pos_fname
          pos.pos_lnum cpos
      in
      Log.debug "%s@\n%s@\n" file_line msg
    in
    fun buf ->
      Log.debug "parsing      ...@\n";
      let provider () =
        let tok = Lexer.token buf in
        let start, stop = Sedlexing.lexing_positions buf in
        (tok, start, stop)
      in
      try Ok (parser provider) with
      | Types.Parse_fail msg
      | Lexer.Illegal_escape msg
      | Lexer.Unknown_operator msg ->
        print_err buf msg;
        Error msg
      | Lexer.Unexpected_character msg ->
        print_err buf msg;
        Error "unknown operator"
      | Menhir_parser.Error ->
        let msg = "unexpected token" in
        print_err buf msg;
        Error msg

  let from_file ~filename =
    let chan = open_in filename in
    let result =
      Fun.protect
        ~finally:(fun () -> close_in chan)
        (fun () ->
          let lb = Sedlexing.Utf8.from_channel chan in
          Sedlexing.set_filename lb filename;
          from_lexbuf lb )
    in
    result

  let from_string s = from_lexbuf (Sedlexing.Utf8.from_string s)

  let from_channel c = from_lexbuf (Sedlexing.Utf8.from_channel c)
end

module Script = Make (struct
  type t = Symbolic.script

  let rule = Menhir_parser.script
end)

module Module = Make (struct
  type t = Symbolic.modul

  let rule = Menhir_parser.modul
end)

module Inline_module = Make (struct
  type t = Symbolic.modul

  let rule = Menhir_parser.inline_module
end)
