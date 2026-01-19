(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Symbolic_global_collection.global =
  { value : Symbolic_value.t
  ; env_id : int
  ; id : int
  }

let value global = global.value

let replace global =
  Symbolic_choice.modify_thread (Thread.replace_global global)

let set_value global value =
  let global = { global with value } in
  replace global
