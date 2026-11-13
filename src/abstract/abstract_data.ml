(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = string option

let value = function
  | Some s -> s
  | None ->
    (* TODO: is this correct? *)
    ""

let size = function None -> 0 | Some s -> String.length s

let drop _data = None

let of_string s = Some s

let to_string s = s
