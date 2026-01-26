(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Smtml.Typed.Bitv128

let of_concrete (v : Concrete_v128.t) : t =
  let a, b = Concrete_v128.to_i64x2 v in
  Smtml.Typed.Bitv64.concat
    (Symbolic_i64.of_concrete a)
    (Symbolic_i64.of_concrete b)
