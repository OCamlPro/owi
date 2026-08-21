(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)
type t

include
  Table_intf.T
    with type reference := Symbolic_value.t Symbolic_ref.t
     and type t := t
