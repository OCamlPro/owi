(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Concrete_value.t Concrete_ref.t Iarray.t

let get (e : t) i = Iarray.get e i

let size (e : t) = Iarray.length e

let drop (_e : t) = Iarray.of_list []

let init l = Iarray.of_list l
