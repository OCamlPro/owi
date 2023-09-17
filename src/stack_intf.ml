(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module type Value = Value_intf.T

module type S = sig
  type vbool

  type int32

  type int64

  type float32

  type float64

  type ref_value

  type value

  type t = value list

  val empty : t

  val pp : Format.formatter -> t -> unit

  (** pop operations *)

  val drop : t -> t

  val drop_n : 'a list -> int -> 'a list

  val pop : t -> value * t

  val pop_n : t -> int -> t * t

  val keep : t -> int -> t

  val pop_bool : t -> vbool * t

  val pop_i32 : t -> int32 * t

  (* val pop_i32_to_char : t -> char * t *)

  (* val pop_i32_to_int : t -> int * t *)

  (* val pop_ui32_to_int : t -> int * t *)

  val pop2_i32 : t -> (int32 * int32) * t

  val pop_i64 : t -> int64 * t

  val pop2_i64 : t -> (int64 * int64) * t

  val pop_f32 : t -> float32 * t

  val pop2_f32 : t -> (float32 * float32) * t

  val pop_f64 : t -> float64 * t

  val pop2_f64 : t -> (float64 * float64) * t

  val pop_ref : t -> value * t

  (* val pop_is_null : t -> bool * t *)

  val pop_as_ref : t -> ref_value * t

  (* val pop_as_externref : 'a Type.Id.t -> 'b t -> 'a * 'b t *)

  (** push operations *)

  val push : t -> value -> t

  val push_const_bool : t -> Stdlib.Bool.t -> t

  val push_bool : t -> vbool -> t

  val push_i32 : t -> int32 -> t

  val push_const_i32 : t -> Int32.t -> t

  val push_i32_of_int : t -> int -> t

  val push_i64 : t -> int64 -> t

  val push_const_i64 : t -> Int64.t -> t

  val push_i64_of_int : t -> int -> t

  val push_f32 : t -> float32 -> t

  val push_const_f32 : t -> Float32.t -> t

  val push_f64 : t -> float64 -> t

  val push_const_f64 : t -> Float64.t -> t

  val push_as_externref : t -> 'b Type.Id.t -> 'b -> t

  val push_array : t -> unit Array.t -> t
end
