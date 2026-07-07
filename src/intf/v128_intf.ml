(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  val pp : t Fmt.t

  type boolean

  type i32

  type i64

  type f32

  type f64

  val zero : t

  val of_concrete : Concrete_v128.t -> t

  val of_i32x4 : i32 -> i32 -> i32 -> i32 -> t

  val of_i64x2 : i64 -> i64 -> t

  val of_f32x4 : f32 -> f32 -> f32 -> f32 -> t

  val of_f64x2 : f64 -> f64 -> t

  val to_i32x4 : t -> i32 * i32 * i32 * i32

  val logor : t -> t -> t

  val logand : t -> t -> t

  val lognot : t -> t

  val andnot : t -> t -> t

  val logxor : t -> t -> t

  val bitselect : t -> t -> t -> t

  val replace_lane8 : int -> i32 -> t -> t

  val replace_lane16 : int -> i32 -> t -> t

  val replace_lane32 : int -> i32 -> t -> t

  val replace_lane64 : int -> i64 -> t -> t

  val extract_lane8 : int -> t -> i32

  val extract_lane16 : int -> t -> i32

  val extract_lane32 : int -> t -> i32

  val extract_lane64 : int -> t -> i64

  val any_true : t -> boolean

  module I8x16 : sig
    val eq : t -> t -> t

    val ne : t -> t -> t

    val splat : i32 -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t

    val abs : t -> t

    val neg : t -> t

    val popcnt : t -> t

    val all_true : t -> i32

    val lt_s : t -> t -> t

    val lt_u : t -> t -> t

    val gt_s : t -> t -> t

    val gt_u : t -> t -> t

    val le_s : t -> t -> t

    val le_u : t -> t -> t

    val ge_s : t -> t -> t

    val ge_u : t -> t -> t

    val shl : i32 -> t -> t

    val shr_s : i32 -> t -> t

    val shr_u : i32 -> t -> t

    val min_s : t -> t -> t

    val min_u : t -> t -> t

    val max_s : t -> t -> t

    val max_u : t -> t -> t

    val add_sat_s : t -> t -> t

    val add_sat_u : t -> t -> t

    val sub_sat_s : t -> t -> t

    val sub_sat_u : t -> t -> t

    val avgr_u : t -> t -> t

    val narrow_i16x8_s : t -> t -> t

    val narrow_i16x8_u : t -> t -> t

    val bitselect : t -> t -> t -> t

    val swizzle : t -> t -> t

    val extract_lane_s : int -> t -> i32

    val extract_lane_u : int -> t -> i32

    val replace_lane : int -> i32 -> t -> t

    val shuffle : int array -> t -> t -> t
  end

  module I16x8 : sig
    val eq : t -> t -> t

    val ne : t -> t -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val abs : t -> t

    val neg : t -> t

    val all_true : t -> i32

    val lt_s : t -> t -> t

    val lt_u : t -> t -> t

    val gt_s : t -> t -> t

    val gt_u : t -> t -> t

    val le_s : t -> t -> t

    val le_u : t -> t -> t

    val ge_s : t -> t -> t

    val ge_u : t -> t -> t

    val min_s : t -> t -> t

    val min_u : t -> t -> t

    val max_s : t -> t -> t

    val max_u : t -> t -> t

    val add_sat_s : t -> t -> t

    val add_sat_u : t -> t -> t

    val sub_sat_s : t -> t -> t

    val sub_sat_u : t -> t -> t

    val q15mulr_sat_s : t -> t -> t

    val shl : i32 -> t -> t

    val shr_s : i32 -> t -> t

    val shr_u : i32 -> t -> t

    val avgr_u : t -> t -> t

    val narrow_i32x4_s : t -> t -> t

    val narrow_i32x4_u : t -> t -> t

    val extend_low_i8x16_s : t -> t

    val extend_low_i8x16_u : t -> t

    val extend_high_i8x16_s : t -> t

    val extend_high_i8x16_u : t -> t

    val extmul_low_i8x16_s : t -> t -> t

    val extmul_low_i8x16_u : t -> t -> t

    val extmul_high_i8x16_s : t -> t -> t

    val extmul_high_i8x16_u : t -> t -> t

    val extadd_pairwise_i8x16_s : t -> t

    val extadd_pairwise_i8x16_u : t -> t

    val extract_lane_s : int -> t -> i32

    val extract_lane_u : int -> t -> i32

    val replace_lane : int -> i32 -> t -> t

    val splat : i32 -> t
  end

  module I32x4 : sig
    val eq : t -> t -> t

    val ne : t -> t -> t

    val splat : i32 -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val neg : t -> t

    val abs : t -> t

    val all_true : t -> i32

    val lt_s : t -> t -> t

    val lt_u : t -> t -> t

    val gt_s : t -> t -> t

    val gt_u : t -> t -> t

    val le_s : t -> t -> t

    val le_u : t -> t -> t

    val ge_s : t -> t -> t

    val ge_u : t -> t -> t

    val min_s : t -> t -> t

    val min_u : t -> t -> t

    val max_s : t -> t -> t

    val max_u : t -> t -> t

    val shl : i32 -> t -> t

    val shr_s : i32 -> t -> t

    val shr_u : i32 -> t -> t

    val extend_low_i16x8_s : t -> t

    val extend_low_i16x8_u : t -> t

    val extend_high_i16x8_s : t -> t

    val extend_high_i16x8_u : t -> t

    val extmul_low_i16x8_s : t -> t -> t

    val extmul_low_i16x8_u : t -> t -> t

    val extmul_high_i16x8_s : t -> t -> t

    val extmul_high_i16x8_u : t -> t -> t

    val extadd_pairwise_i16x8_s : t -> t

    val extadd_pairwise_i16x8_u : t -> t

    val dot_i16x8_s : t -> t -> t

    val trunc_sat_f32x4_s : t -> t

    val trunc_sat_f32x4_u : t -> t

    val trunc_sat_f64x2_s_zero : t -> t

    val trunc_sat_f64x2_u_zero : t -> t

    val extract_lane : int -> t -> i32

    val replace_lane : int -> i32 -> t -> t
  end

  module I64x2 : sig
    val eq : t -> t -> t

    val ne : t -> t -> t

    val splat : i64 -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val neg : t -> t

    val abs : t -> t

    val all_true : t -> i32

    val lt_s : t -> t -> t

    val gt_s : t -> t -> t

    val le_s : t -> t -> t

    val ge_s : t -> t -> t

    val shl : i32 -> t -> t

    val shr_s : i32 -> t -> t

    val shr_u : i32 -> t -> t

    val extend_low_i32x4_s : t -> t

    val extend_low_i32x4_u : t -> t

    val extend_high_i32x4_s : t -> t

    val extend_high_i32x4_u : t -> t

    val extmul_low_i32x4_s : t -> t -> t

    val extmul_low_i32x4_u : t -> t -> t

    val extmul_high_i32x4_s : t -> t -> t

    val extmul_high_i32x4_u : t -> t -> t

    val extract_lane : int -> t -> i64

    val replace_lane : int -> i64 -> t -> t
  end

  module F32x4 : sig
    val abs : t -> t

    val neg : t -> t

    val sqrt : t -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val div : t -> t -> t

    val min : t -> t -> t

    val max : t -> t -> t

    val pmin : t -> t -> t

    val pmax : t -> t -> t

    val eq : t -> t -> t

    val ne : t -> t -> t

    val lt : t -> t -> t

    val gt : t -> t -> t

    val le : t -> t -> t

    val ge : t -> t -> t

    val ceil : t -> t

    val floor : t -> t

    val trunc : t -> t

    val nearest : t -> t

    val splat : f32 -> t

    val convert_i32x4_s : t -> t

    val convert_i32x4_u : t -> t

    val convert_low_i32x4_s : t -> t

    val convert_low_i32x4_u : t -> t

    val convert_high_i32x4_s : t -> t

    val convert_high_i32x4_u : t -> t

    val demote_f64x2_zero : t -> t

    val extract_lane : int -> t -> f32

    val replace_lane : int -> f32 -> t -> t
  end

  module F64x2 : sig
    val abs : t -> t

    val neg : t -> t

    val sqrt : t -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val div : t -> t -> t

    val min : t -> t -> t

    val max : t -> t -> t

    val pmin : t -> t -> t

    val pmax : t -> t -> t

    val eq : t -> t -> t

    val ne : t -> t -> t

    val lt : t -> t -> t

    val gt : t -> t -> t

    val le : t -> t -> t

    val ge : t -> t -> t

    val ceil : t -> t

    val floor : t -> t

    val trunc : t -> t

    val nearest : t -> t

    val splat : f64 -> t

    val convert_low_i32x4_s : t -> t

    val convert_low_i32x4_u : t -> t

    val convert_high_i32x4_s : t -> t

    val convert_high_i32x4_u : t -> t

    val promote_low_f32x4 : t -> t

    val extract_lane : int -> t -> f64

    val replace_lane : int -> f64 -> t -> t
  end
end
