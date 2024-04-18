(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Various conversion functions between i32, i64, f32 and f64. *)

module Int32 : sig
  type t = int32

  val wrap_i64 : int64 -> t

  val trunc_f32_s : Float32.t -> t

  val trunc_f32_u : Float32.t -> t

  val trunc_f64_s : Float64.t -> t

  val trunc_f64_u : Float64.t -> t

  val trunc_sat_f32_s : Float32.t -> t

  val trunc_sat_f32_u : Float32.t -> t

  val trunc_sat_f64_s : Float64.t -> t

  val trunc_sat_f64_u : Float64.t -> t

  val reinterpret_f32 : Float32.t -> t
end

module Int64 : sig
  type t = int64

  val extend_i32_s : int32 -> t

  val extend_i32_u : int32 -> t

  val trunc_f32_s : Float32.t -> t

  val trunc_f32_u : Float32.t -> t

  val trunc_f64_s : Float64.t -> t

  val trunc_f64_u : Float64.t -> t

  val trunc_sat_f32_s : Float32.t -> t

  val trunc_sat_f32_u : Float32.t -> t

  val trunc_sat_f64_s : Float64.t -> t

  val trunc_sat_f64_u : Float64.t -> t

  val reinterpret_f64 : Float64.t -> t
end

module Float32 : sig
  type t = Float32.t

  val demote_f64 : Float64.t -> t

  val convert_i32_s : Int32.t -> t

  val convert_i32_u : Int32.t -> t

  val convert_i64_s : Int64.t -> t

  val convert_i64_u : Int64.t -> t

  val reinterpret_i32 : Int32.t -> t
end

module Float64 : sig
  type t = Float64.t

  val promote_f32 : Float32.t -> t

  val convert_i32_s : Int32.t -> t

  val convert_i32_u : Int32.t -> t

  val convert_i64_s : Int64.t -> t

  val convert_i64_u : Int64.t -> t

  val reinterpret_i64 : Int64.t -> t
end
