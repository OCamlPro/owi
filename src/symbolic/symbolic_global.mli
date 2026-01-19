(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Symbolic_global0.t

include
  Global_intf.T
    with type value := Symbolic_value.t
     and type t := t
     and type 'a choice := 'a Symbolic_choice.t

val replace : t -> unit Symbolic_choice.t

val of_concrete : env_id:int -> id:int -> Concrete_global.t -> t
