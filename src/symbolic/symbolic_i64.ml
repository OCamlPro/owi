(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Smtml.Typed.Bitv64

(* See: https://github.com/formalsec/smtml/pull/629 *)
(* in the future, we might change to rotate_right but it'll require to use a select_i32 (which would not cost anything in the case of an integer) ? *)
let rotate_right = ext_rotate_right

let rotate_left = ext_rotate_left

let eq_concrete (e : t) (c : Int64.t) : Symbolic_boolean.t =
  let c = of_int64 c in
  Smtml.Typed.Bitv64.eq c e

let min_int = of_int64 Int64.min_int

let eqz (v : t) = eq v zero

let ( = ) = eq

let ( + ) = add

let ( * ) = mul

let ( / ) = div
