(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Symbolic_global_collection.global =
  { value : Symbolic_value.t
  ; env_id : int
  ; id : int
  }

let value v = v.value

let replace ~env_id ~id v =
  Symbolic_choice.modify_thread (fun thread ->
    Thread.replace_global thread ~env_id ~id v )

let set_value ({ env_id; id; _ } as v) value =
  let v = { v with value } in
  replace ~env_id ~id v
