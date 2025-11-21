(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type func =
  | Wasm of int * Binary.Func.t * Env_id.t
  | Extern of int

let fresh =
  let r = ref ~-1 in
  fun () ->
    incr r;
    !r

let wasm func env : func = Wasm (fresh (), func, env)

let extern f : func = Extern f

type 'f t =
  | Wat of Text.Module.t
  | Wast of Wast.script
  | Wasm of Binary.Module.t
  | Extern of 'f Extern.Module.t
