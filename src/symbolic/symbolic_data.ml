(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Concrete_data.t

let value data = data.Concrete_data.value

let size data = String.length data.Concrete_data.value

let drop data = data.Concrete_data.value <- ""
