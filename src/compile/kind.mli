(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type func = private
  | Wasm of int * Binary.Func.t * Env_id.t
  | Extern of Func_id.t

val wasm : Binary.Func.t -> Env_id.t -> func

val extern : Func_id.t -> func

type 'f t =
  | Wat of Text.Module.t
  | Wast of Wast.script
  | Wasm of Binary.Module.t
  | Extern of 'f Extern.Module.t
