(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to execute a full Wasm script. *)

val load_func_from_module :
     'f Link.state
  -> Link.StringMap.key option
  -> Link.StringMap.key
  -> (Func_intf.t * Env_id.t) Result.t

(** execute a Wasm script *)
val exec : no_exhaustion:bool -> optimize:bool -> Text.script -> unit Result.t
