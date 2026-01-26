(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type concrete

  type boolean

  type f32

  type f64

  val zero : t

  val clz : t -> t

  val ctz : t -> t

  val popcnt : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val unsigned_div : t -> t -> t

  val rem : t -> t -> t

  val unsigned_rem : t -> t -> t

  val logand : t -> t -> t

  val logor : t -> t -> t

  val logxor : t -> t -> t

  val shl : t -> t -> t

  val ashr : t -> t -> t

  val lshr : t -> t -> t

  val rotate_left : t -> t -> t

  val rotate_right : t -> t -> t

  val eq_concrete : t -> concrete -> boolean

  val eq : t -> t -> boolean

  val ne : t -> t -> boolean

  val lt : t -> t -> boolean

  val gt : t -> t -> boolean

  val lt_u : t -> t -> boolean

  val gt_u : t -> t -> boolean

  val le : t -> t -> boolean

  val ge : t -> t -> boolean

  val le_u : t -> t -> boolean

  val ge_u : t -> t -> boolean

  val trunc_f32_s : f32 -> t Result.t

  val trunc_f32_u : f32 -> t Result.t

  val trunc_f64_s : f64 -> t Result.t

  val trunc_f64_u : f64 -> t Result.t

  val trunc_sat_f32_s : f32 -> t

  val trunc_sat_f32_u : f32 -> t

  val trunc_sat_f64_s : f64 -> t

  val trunc_sat_f64_u : f64 -> t

  val extend_s : int -> t -> t
end
