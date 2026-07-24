(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type func = private
  | Wasm of
      { func : Binary.Func.t
      ; modul : int
      }
  | Extern of { idx : int }

val wasm : Binary.Func.t -> modul:int -> func

val extern : int -> func

type 'f t =
  | Wat of Text.Module.t
  | Wast of Wast.script
  | Wasm of Binary.Module.t
  | Extern of 'f
