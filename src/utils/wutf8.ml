(* SPDX-License-Identifier: Apache-2.0 *)
(* Copyright 2017 WebAssembly Community Group participants *)
(* This file is originally from the WebAssembly reference interpreter available at https://github.com/WebAssembly/spec/tree/main/interpreter *)

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Modified by the Owi programmers *)

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
