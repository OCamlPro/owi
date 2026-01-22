(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Smtml.Typed.bitv128 Smtml.Typed.t

let of_concrete (v : Concrete_v128.t) : t =
  let a, b = Concrete_v128.to_i64x2 v in
  Smtml.Typed.Bitv64.concat
    (Symbolic_i64.of_concrete a)
    (Symbolic_i64.of_concrete b)

let zero : t = of_concrete Concrete_v128.zero

let of_i32x4 a b c d =
  Smtml.Typed.Bitv64.concat
    (Smtml.Typed.Bitv32.concat a b)
    (Smtml.Typed.Bitv32.concat c d)

let to_i32x4 v =
  let a = Smtml.Typed.Bitv128.extract v ~low:12 ~high:16 in
  let b = Smtml.Typed.Bitv128.extract v ~low:8 ~high:12 in
  let c = Smtml.Typed.Bitv128.extract v ~low:4 ~high:8 in
  let d = Smtml.Typed.Bitv128.extract v ~low:0 ~high:4 in
  (a, b, c, d)

let of_i64x2 a b = Smtml.Typed.Bitv64.concat a b

let to_i64x2 v =
  let a = Smtml.Typed.Bitv128.extract v ~low:8 ~high:16 in
  let b = Smtml.Typed.Bitv128.extract v ~low:0 ~high:8 in
  (a, b)

let pp ppf v = Smtml.Typed.Bitv128.pp ppf v
