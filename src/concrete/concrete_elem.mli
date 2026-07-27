(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

include Elem_intf.T with type reference := Concrete_ref.t and type t := t
