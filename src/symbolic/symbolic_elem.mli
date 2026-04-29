(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Elem_intf.T with type reference := Symbolic_ref.t and type t = Concrete_elem.t
