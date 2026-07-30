(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type func =
  | Wasm of Binary.Func.t
  | Extern of int

let pp_func ppf = function
  | Wasm _f -> Fmt.pf ppf "Wasm <code>"
  | Extern n -> Fmt.pf ppf "Extern %d" n

type 'f t =
  | Wat of Text.Module.t
  | Wast of Wast.script
  | Wasm of Binary.Module.t
  | Extern of 'f
