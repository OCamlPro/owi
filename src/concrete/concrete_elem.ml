(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = { mutable value : Concrete_ref.t Iarray.t }

let get (e : t) i = Iarray.get e.value i

let size (e : t) = Iarray.length e.value

let drop (e : t) = e.value <- [||]
