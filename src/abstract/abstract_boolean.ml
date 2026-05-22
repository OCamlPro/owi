(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.boolean

let true_ ctx = Abstract_domain.Boolean_Forward.true_ ctx

let false_ ctx = Abstract_domain.Boolean_Forward.false_ ctx

let of_bool ctx = function true -> true_ ctx | false -> false_ ctx

let equal b1 b2 = Abstract_domain.Boolean.equal b1 b2

let pp _ = assert false

let not ctx b = Abstract_domain.Boolean_Forward.not ctx b

let or_ ctx b1 b2 = Abstract_domain.Boolean_Forward.( || ) ctx b1 b2

let and_ ctx b1 b2 = Abstract_domain.Boolean_Forward.( && ) ctx b1 b2
