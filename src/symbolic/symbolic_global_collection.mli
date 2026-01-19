(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type global =
  { value : Symbolic_value.t
  ; env_id : int
  ; id : int
  }

val global_of_concrete : env_id:int -> id:int -> Concrete_global.t -> global

include Collection.S with type symbolic := global
