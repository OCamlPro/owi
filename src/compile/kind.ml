(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type func =
  | Wasm of
      { func : Binary.Func.t
      ; idx : int
      }
  | Extern of { idx : int }

let wasm func idx : func = Wasm { func; idx }

let extern idx : func = Extern { idx }

type 'f t =
  | Wat of Text.Module.t
  | Wast of Wast.script
  | Wasm of Binary.Module.t
  | Extern of 'f Extern.Module.t
