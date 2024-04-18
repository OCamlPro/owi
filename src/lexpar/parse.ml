(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

module Make (M : sig
  type t

  val rule : (Lexing.lexbuf -> Menhir_parser.token) -> Lexing.lexbuf -> t
end) =
struct
  let from_lexbuf =
    let parser = MenhirLib.Convert.Simplified.traditional2revised M.rule in
    fun buf ->
      Log.debug0 "parsing      ...@\n";
      let provider () =
        let tok = Lexer.token buf in
        let start, stop = Sedlexing.lexing_positions buf in
        (tok, start, stop)
      in
      try Ok (parser provider) with
      | Types.Parse_fail msg -> Error (`Parse_fail msg)
      | Lexer.Illegal_escape msg -> Error (`Illegal_escape msg)
      | Lexer.Unknown_operator msg -> Error (`Lexer_unknown_operator msg)
      | Lexer.Unexpected_character msg -> Error (`Lexer_unknown_operator msg)
      | Menhir_parser.Error -> Error `Unexpected_token

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

module Script = Make (struct
  type t = Text.script

  let rule = Menhir_parser.script
end)

module Module = Make (struct
  type t = Text.modul

  let rule = Menhir_parser.modul
end)

module Inline_module = Make (struct
  type t = Text.modul

  let rule = Menhir_parser.inline_module
end)
