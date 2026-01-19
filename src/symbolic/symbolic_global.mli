(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

include Global_intf.T with type value := Symbolic_value.t and type t := t

val of_concrete : Concrete_global.t -> t

module Collection :
  Collection.S with type concrete := Concrete_global.t and type symbolic := t
