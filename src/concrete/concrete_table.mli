(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime table *)

type t

include
  Table_intf.T
    with type reference := Concrete_value.t Concrete_ref.t
     and type t := t
