(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Smtml.Typed.Bitv128

let of_concrete (v : Concrete_v128.t) : t =
  let a, b = Concrete_v128.to_i64x2 v in
  Smtml.Typed.Bitv128.of_int64x2 a b

let of_f32x4 _ = assert false

let of_f64x2 _ = assert false

(* TODO: implement in Smt.ml *)
module F32x4 = struct
  type nonrec t = t

  let abs _ = assert false

  let neg _ = assert false

  let sqrt _ = assert false

  let add _ = assert false

  let sub _ = assert false

  let mul _ = assert false

  let div _ = assert false

  let min _ = assert false

  let max _ = assert false

  let pmin _ = assert false

  let pmax _ = assert false

  let eq _ = assert false

  let ne _ = assert false

  let lt _ = assert false

  let gt _ = assert false

  let le _ = assert false

  let ge _ = assert false

  let ceil _ = assert false

  let floor _ = assert false

  let trunc _ = assert false

  let nearest _ = assert false

  let splat _ = assert false

  let convert_i32x4_s _ = assert false

  let convert_i32x4_u _ = assert false

  let convert_low_i32x4_s _ = assert false

  let convert_low_i32x4_u _ = assert false

  let convert_high_i32x4_s _ = assert false

  let convert_high_i32x4_u _ = assert false

  let demote_f64x2_zero _ = assert false

  let extract_lane _ = assert false

  let replace_lane _ = assert false
end

(* TODO: implement in Smt.ml *)
module F64x2 = struct
  type nonrec t = t

  let abs _ = assert false

  let neg _ = assert false

  let sqrt _ = assert false

  let add _ = assert false

  let sub _ = assert false

  let mul _ = assert false

  let div _ = assert false

  let min _ = assert false

  let max _ = assert false

  let pmin _ = assert false

  let pmax _ = assert false

  let eq _ = assert false

  let ne _ = assert false

  let lt _ = assert false

  let gt _ = assert false

  let le _ = assert false

  let ge _ = assert false

  let ceil _ = assert false

  let floor _ = assert false

  let trunc _ = assert false

  let nearest _ = assert false

  let splat _ = assert false

  let convert_low_i32x4_s _ = assert false

  let convert_low_i32x4_u _ = assert false

  let convert_high_i32x4_s _ = assert false

  let convert_high_i32x4_u _ = assert false

  let promote_low_f32x4 _ = assert false

  let extract_lane _ = assert false

  let replace_lane _ = assert false
end

module I8x16 = struct
  include I8x16

  let splat _ =
    (* TODO: should convert the i32 to i8 and call the Smtml one *)
    assert false

  let ne _ = assert false

  let abs _ = assert false

  let neg _ = assert false

  let popcnt _ = assert false

  let all_true _ = assert false

  let lt_s _ = assert false

  let lt_u _ = assert false

  let gt_s _ = assert false

  let gt_u _ = assert false

  let le_s _ = assert false

  let le_u _ = assert false

  let ge_s _ = assert false

  let ge_u _ = assert false

  let shl _ = assert false

  let shr_s _ = assert false

  let shr_u _ = assert false

  let min_s _ = assert false

  let min_u _ = assert false

  let max_s _ = assert false

  let max_u _ = assert false

  let add_sat_s _ = assert false

  let add_sat_u _ = assert false

  let sub_sat_s _ = assert false

  let sub_sat_u _ = assert false

  let avgr_u _ = assert false

  let narrow_i16x8_s _ = assert false

  let narrow_i16x8_u _ = assert false

  let bitselect _ = assert false

  let swizzle _ = assert false

  let extract_lane_s _ = assert false

  let extract_lane_u _ = assert false

  let replace_lane _ = assert false

  let shuffle _ = assert false
end

module I16x8 = struct
  include I16x8

  let splat _ =
    (* TODO: should convert the i32 to i8 and call the Smtml one *)
    assert false

  let ne _ = assert false

  let mul _ = assert false

  let abs _ = assert false

  let neg _ = assert false

  let all_true _ = assert false

  let lt_s _ = assert false

  let lt_u _ = assert false

  let gt_s _ = assert false

  let gt_u _ = assert false

  let le_s _ = assert false

  let le_u _ = assert false

  let ge_s _ = assert false

  let ge_u _ = assert false

  let min_s _ = assert false

  let min_u _ = assert false

  let max_s _ = assert false

  let max_u _ = assert false

  let add_sat_s _ = assert false

  let add_sat_u _ = assert false

  let sub_sat_s _ = assert false

  let sub_sat_u _ = assert false

  let q15mulr_sat_s _ = assert false

  let shl _ = assert false

  let shr_s _ = assert false

  let shr_u _ = assert false

  let avgr_u _ = assert false

  let narrow_i32x4_s _ = assert false

  let narrow_i32x4_u _ = assert false

  let extend_low_i8x16_s _ = assert false

  let extend_low_i8x16_u _ = assert false

  let extend_high_i8x16_s _ = assert false

  let extend_high_i8x16_u _ = assert false

  let extmul_low_i8x16_s _ = assert false

  let extmul_low_i8x16_u _ = assert false

  let extmul_high_i8x16_s _ = assert false

  let extmul_high_i8x16_u _ = assert false

  let extadd_pairwise_i8x16_s _ = assert false

  let extadd_pairwise_i8x16_u _ = assert false

  let extract_lane_s _ = assert false

  let extract_lane_u _ = assert false

  let replace_lane _ = assert false
end

module I64x2 = struct
  include I64x2

  let ne _ = assert false

  let mul _ = assert false

  let neg _ = assert false

  let abs _ = assert false

  let all_true _ = assert false

  let lt_s _ = assert false

  let gt_s _ = assert false

  let le_s _ = assert false

  let ge_s _ = assert false

  let shl _ = assert false

  let shr_s _ = assert false

  let shr_u _ = assert false

  let extend_low_i32x4_s _ = assert false

  let extend_low_i32x4_u _ = assert false

  let extend_high_i32x4_s _ = assert false

  let extend_high_i32x4_u _ = assert false

  let extmul_low_i32x4_s _ = assert false

  let extmul_low_i32x4_u _ = assert false

  let extmul_high_i32x4_s _ = assert false

  let extmul_high_i32x4_u _ = assert false

  let extract_lane _ = assert false

  let replace_lane _ = assert false
end

module I32x4 = struct
  include I32x4
  (* TODO: implement in Smt.ml *)

  let ne _ = assert false

  let mul _ = assert false

  let neg _ = assert false

  let abs _ = assert false

  let all_true _ = assert false

  let lt_s _ = assert false

  let lt_u _ = assert false

  let gt_s _ = assert false

  let gt_u _ = assert false

  let le_s _ = assert false

  let le_u _ = assert false

  let ge_s _ = assert false

  let ge_u _ = assert false

  let min_s _ = assert false

  let min_u _ = assert false

  let max_s _ = assert false

  let max_u _ = assert false

  let shl _ = assert false

  let shr_s _ = assert false

  let shr_u _ = assert false

  let extend_low_i16x8_s _ = assert false

  let extend_low_i16x8_u _ = assert false

  let extend_high_i16x8_s _ = assert false

  let extend_high_i16x8_u _ = assert false

  let extmul_low_i16x8_s _ = assert false

  let extmul_low_i16x8_u _ = assert false

  let extmul_high_i16x8_s _ = assert false

  let extmul_high_i16x8_u _ = assert false

  let extadd_pairwise_i16x8_s _ = assert false

  let extadd_pairwise_i16x8_u _ = assert false

  let dot_i16x8_s _ = assert false

  let trunc_sat_f32x4_s _ = assert false

  let trunc_sat_f32x4_u _ = assert false

  let trunc_sat_f32x4_s_zero _ = assert false

  let trunc_sat_f32x4_u_zero _ = assert false

  let trunc_sat_f64x2_s_zero _ = assert false

  let trunc_sat_f64x2_u_zero _ = assert false

  let extract_lane _ = assert false

  let replace_lane _ = assert false
end

(* TODO: implement in Smt.ml *)
let lognot _ = assert false

let logxor _ = assert false

let andnot _ = assert false

let bitselect _ = assert false

let replace_lane8 _ = assert false

let replace_lane16 _ = assert false

let replace_lane32 _ = assert false

let replace_lane64 _ = assert false

let extract_lane8 _ = assert false

let extract_lane16 _ = assert false

let extract_lane32 _ = assert false

let extract_lane64 _ = assert false
