(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.boolean

let true_ ctx = Abstract_domain.Boolean_Forward.true_ ctx

let false_ ctx = Abstract_domain.Boolean_Forward.false_ ctx

let of_bool ctx = function true -> true_ ctx | false -> false_ ctx

let pp = Abstract_domain.boolean_pretty

let not ctx b = Abstract_domain.Boolean_Forward.not ctx b

let or_ ctx b1 b2 = Abstract_domain.Boolean_Forward.( || ) ctx b1 b2

let and_ ctx b1 b2 = Abstract_domain.Boolean_Forward.( && ) ctx b1 b2

let eq ctx b1 b2 =
  let both_true = and_ ctx b1 b2 in
  let both_false = and_ ctx (not ctx b1) (not ctx b2) in
  or_ ctx both_true both_false

let unknown ctx = Abstract_domain.boolean_unknown ctx

let can_be_true ctx b =
  match Abstract_domain.query_boolean ctx b with
  | True | Top -> true
  | False | Bottom -> false
