(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Link_env.data

let value data = data.Link_env.value

let size data = String.length data.Link_env.value
