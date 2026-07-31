(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type func =
  | Wasm of Binary.Func.t
  | Extern of int

val pp_func : func Fmt.t

type 'f t =
  | Wat of Text.Module.t
  | Wast of Wast.script
  | Wasm of Binary.Module.t
  | Extern of 'f
