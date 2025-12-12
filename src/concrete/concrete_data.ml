(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = { mutable value : string }

let value data = data.value

let size data = String.length data.value

let drop data = data.value <- ""
