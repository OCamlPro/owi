(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

open Syntax

module Make (M : sig
  type t

  val rule : (Lexing.lexbuf -> Text_parser.token) -> Lexing.lexbuf -> t
end) =
struct
  let from_lexbuf =
    let parser = MenhirLib.Convert.Simplified.traditional2revised M.rule in
    fun buf ->
      Log.debug0 "parsing      ...@\n";
      let provider () =
        let tok = Text_lexer.token buf in
        let start, stop = Sedlexing.lexing_positions buf in
        (tok, start, stop)
      in
      try Ok (parser provider) with
      | Types.Parse_fail msg -> Error (`Parse_fail msg)
      | Text_lexer.Illegal_escape msg -> Error (`Illegal_escape msg)
      | Text_lexer.Unknown_operator msg -> Error (`Lexer_unknown_operator msg)
      | Text_lexer.Unexpected_character msg ->
        Error (`Lexer_unknown_operator msg)
      | Text_parser.Error -> Error `Unexpected_token

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
    type t = Text.script

    let rule = Text_parser.script
  end)

  module Module = Make (struct
    type t = Text.modul

    let rule = Text_parser.modul
  end)

  module Inline_module = Make (struct
    type t = Text.modul

    let rule = Text_parser.inline_module
  end)
end

module Binary = struct
  module Module = Binary_parser
end

let guess_from_file file =
  match Fpath.get_ext ~multi:false file with
  | ".wat" ->
    let+ m = Text.Module.from_file file in
    Either.Left (Either.Left m)
  | ".wast" ->
    let+ m = Text.Script.from_file file in
    Either.Left (Either.Right m)
  | ".wasm" ->
    let+ m = Binary.Module.from_file file in
    Either.Right m
  | ext -> Error (`Unsupported_file_extension ext)
