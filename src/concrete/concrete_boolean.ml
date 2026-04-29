(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = bool

let false_ = false

let true_ = true

let[@inline] to_bool b = b

let[@inline] of_bool c = c

let[@inline] not b = not b

let[@inline] and_ x y = x && y

let[@inline] or_ x y = x || y

let[@inline] pp fmt b = Fmt.bool fmt b
