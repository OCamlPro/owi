(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2025 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: make it opaque *)
type t = { mutable value : Concrete_ref.t array }

include Elem_intf.T with type reference := Concrete_ref.t and type t := t
