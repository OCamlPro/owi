(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'extern func =
  | Wasm of Binary.Func.t
  | Extern of 'extern

let pp_func ppf = function
  | Wasm _f -> Fmt.pf ppf "Wasm <code>"
  | Extern _f -> Fmt.pf ppf "Extern <code>"

type 'f t =
  | Wat of Text.Module.t
  | Wast of Wast.script
  | Wasm of Binary.Module.t
  | Extern of 'f
