(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'extern_func t =
  | Wat of Text.modul
  | Wast of Text.script
  | Wasm of Binary.Module.t
  | Ocaml of 'extern_func Link.extern_module
