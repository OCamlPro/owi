(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Smtml.Typed.Bitv64

let eq_concrete (e : t) (c : Int64.t) : Symbolic_boolean.t =
  let c = of_int64 c in
  Smtml.Typed.Bitv64.eq c e
