(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime memory *)
type t

include
  Memory_intf.T
    with module Value := Concrete_value
     and module Choice := Concrete_choice
     and type t := t

val get_limits : t -> Text.limits

val init : Text.limits -> t
