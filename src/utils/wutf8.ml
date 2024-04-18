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

(* Taken and modified from https://github.com/WebAssembly/spec/tree/main/interpreter *)

exception Utf8

let string_implode cs =
  let buf = Buffer.create 80 in
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf

let con n = 0x80 lor (n land 0x3f)

let rec encode ns = string_implode (List.map Char.chr (encode' ns))

and encode' = function
  | [] -> []
  | n :: _ns when n < 0 -> raise Utf8
  | n :: ns when n < 0x80 -> n :: encode' ns
  | n :: ns when n < 0x800 -> (0xc0 lor (n lsr 6)) :: con n :: encode' ns
  | n :: ns when n < 0x10000 ->
    (0xe0 lor (n lsr 12)) :: con (n lsr 6) :: con n :: encode' ns
  | n :: ns when n < 0x110000 ->
    (0xf0 lor (n lsr 18))
    :: con (n lsr 12)
    :: con (n lsr 6)
    :: con n :: encode' ns
  | _ -> raise Utf8

let check_utf8 s =
  let open Uutf in
  let decoder = decoder ~encoding:`UTF_8 (`String s) in
  let rec loop () =
    match decode decoder with
    | `Malformed s -> Error (`Malformed_utf8_encoding s)
    | `Await -> assert false
    | `End -> Ok ()
    | `Uchar _ -> loop ()
  in
  loop ()
