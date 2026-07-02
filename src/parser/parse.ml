(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

open Syntax

module Make (M : sig
  type t

  val rule : (Lexing.lexbuf -> Text_parser.token) -> Lexing.lexbuf -> t
end) =
struct
  let pp_pos fmt ((p1, p2) : Lexing.position * Lexing.position) =
    if p1.pos_lnum = p2.pos_lnum then
      if p1.pos_cnum = p2.pos_cnum then
        Fmt.pf fmt "line %d, character %d" p1.pos_lnum (p1.pos_cnum - p1.pos_bol)
      else
        Fmt.pf fmt "line %d, character %d-%d" p1.pos_lnum
          (p1.pos_cnum - p1.pos_bol) (p2.pos_cnum - p2.pos_bol)
    else
      Fmt.pf fmt "line %d, character %d to line %d, character %d" p1.pos_lnum
        (p1.pos_cnum - p1.pos_bol) p2.pos_lnum (p2.pos_cnum - p2.pos_bol)

  let from_lexbuf =
    let parser = MenhirLib.Convert.Simplified.traditional2revised M.rule in
    fun buf ->
      Log.info (fun m -> m "parsing      ...");
      let provider () =
        let tok = Text_lexer.token buf in
        let start, stop = Sedlexing.lexing_positions buf in
        (tok, start, stop)
      in
      try Ok (parser provider) with
      | Text.Parse_fail msg -> Error (`Parse_fail msg)
      | Text_lexer.Empty_annotation_id -> Error `Empty_annotation_id
      | Text_lexer.Empty_identifier -> Error `Empty_identifier
      | Text_lexer.Illegal_escape msg -> Error (`Illegal_escape msg)
      | Text_lexer.Illegal_character msg -> Error (`Lexer_illegal_character msg)
      | Text_lexer.Unclosed_annotation -> Error `Unclosed_annotation
      | Text_lexer.Unclosed_comment -> Error `Unclosed_comment
      | Text_lexer.Unclosed_string -> Error `Unclosed_string
      | Text_lexer.Unknown_operator msg -> Error (`Lexer_unknown_operator msg)
      | Text_parser.Error ->
        let tok, pos1, pos2 = Text_lexer.lexer buf () in
        let msg =
          Fmt.str "%S in %a"
            (Text_keywords.token_to_string tok)
            pp_pos (pos1, pos2)
        in
        Error (`Unexpected_token msg)
      | Sedlexing.MalFormed -> Error (`Malformed_utf8_encoding "")

  let from_file filename =
    let open Syntax in
    let* res =
      Bos.OS.File.with_ic filename
        (fun chan () ->
          let lb = Sedlexing.Utf8.from_channel chan in
          Sedlexing.set_filename lb (Fpath.to_string filename);
          from_lexbuf lb )
        ()
    in
    res

  let from_string s = from_lexbuf (Sedlexing.Utf8.from_string s)

  let from_channel c = from_lexbuf (Sedlexing.Utf8.from_channel c)
end

module Text = struct
  module Script = Make (struct
    type t = Wast.script

    let rule = Text_parser.script
  end)

  module Module = Make (struct
    type t = Text.Module.t

    let rule = Text_parser.modul
  end)

  module Inline_module = Make (struct
    type t = Text.Module.t

    let rule = Text_parser.inline_module
  end)
end

module Binary = struct
  module Module = Binary_parser
end

let guess_from_file file =
  Log.bench_fn "parsing time" @@ fun () ->
  match Fpath.get_ext ~multi:false file with
  | ".wat" ->
    let+ m = Text.Module.from_file file in
    Kind.Wat m
  | ".wast" ->
    let+ m = Text.Script.from_file file in
    Kind.Wast m
  | ".wasm" ->
    let+ m = Binary.Module.from_file file in
    Kind.Wasm m
  | ext -> Error (`Unsupported_file_extension ext)
