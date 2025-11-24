(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Iop = sig
  type num

  type const

  type bool

  type float32

  type float64

  val zero : num

  val clz : num -> num

  val ctz : num -> num

  val popcnt : num -> num

  val add : num -> num -> num

  val sub : num -> num -> num

  val mul : num -> num -> num

  val div : num -> num -> num

  val unsigned_div : num -> num -> num

  val rem : num -> num -> num

  val unsigned_rem : num -> num -> num

  val logand : num -> num -> num

  val logor : num -> num -> num

  val logxor : num -> num -> num

  val shl : num -> num -> num

  val shr_s : num -> num -> num

  val shr_u : num -> num -> num

  val rotl : num -> num -> num

  val rotr : num -> num -> num

  val eq_const : num -> const -> bool

  val eq : num -> num -> bool

  val ne : num -> num -> bool

  val lt : num -> num -> bool

  val gt : num -> num -> bool

  val lt_u : num -> num -> bool

  val gt_u : num -> num -> bool

  val le : num -> num -> bool

  val ge : num -> num -> bool

  val le_u : num -> num -> bool

  val ge_u : num -> num -> bool

  val trunc_f32_s : float32 -> num Result.t

  val trunc_f32_u : float32 -> num Result.t

  val trunc_f64_s : float64 -> num Result.t

  val trunc_f64_u : float64 -> num Result.t

  val trunc_sat_f32_s : float32 -> num

  val trunc_sat_f32_u : float32 -> num

  val trunc_sat_f64_s : float64 -> num

  val trunc_sat_f64_u : float64 -> num

  val extend_s : int -> num -> num
end

module type Fop = sig
  type num

  type bool

  type int32

  type int64

  type same_size_int

  val zero : num

  val abs : num -> num

  val neg : num -> num

  val sqrt : num -> num

  val ceil : num -> num

  val floor : num -> num

  val trunc : num -> num

  val nearest : num -> num

  val add : num -> num -> num

  val sub : num -> num -> num

  val mul : num -> num -> num

  val div : num -> num -> num

  val min : num -> num -> num

  val max : num -> num -> num

  val copy_sign : num -> num -> num

  val eq : num -> num -> bool

  val ne : num -> num -> bool

  val lt : num -> num -> bool

  val gt : num -> num -> bool

  val le : num -> num -> bool

  val ge : num -> num -> bool

  val convert_i32_s : int32 -> num

  val convert_i32_u : int32 -> num

  val convert_i64_s : int64 -> num

  val convert_i64_u : int64 -> num

  val of_bits : same_size_int -> num

  val to_bits : num -> same_size_int
end

type 'a get_ref =
  | Null
  | Ref_value of 'a
  | Type_mismatch

module type T = sig
  type bool

  type int32

  val pp_int32 : Format.formatter -> int32 -> unit

  type int64

  val pp_int64 : Format.formatter -> int64 -> unit

  type float32

  val pp_float32 : Format.formatter -> float32 -> unit

  type float64

  val pp_float64 : Format.formatter -> float64 -> unit

  type v128

  val pp_v128 : Format.formatter -> v128 -> unit

  type ref_value

  val pp_ref_value : Format.formatter -> ref_value -> unit

  type t =
    | I32 of int32
    | I64 of int64
    | F32 of float32
    | F64 of float64
    | V128 of v128
    | Ref of ref_value

  val pp : Format.formatter -> t -> unit

  val const_i32 : Int32.t -> int32

  val const_i64 : Int64.t -> int64

  val const_f32 : Float32.t -> float32

  val const_f64 : Float64.t -> float64

  val const_v128 : V128.t -> v128
  (* TODO ref *)

  val ref_null : Text.heap_type -> t

  val ref_func : Kind.func -> t

  val ref_externref : 'a Type.Id.t -> 'a -> t

  val ref_is_null : ref_value -> bool

  module Ref : sig
    val get_func : ref_value -> Kind.func get_ref

    val get_externref : ref_value -> 'a Type.Id.t -> 'a get_ref
  end

  module Bool : sig
    val const : Bool.t -> bool

    val not : bool -> bool

    val or_ : bool -> bool -> bool

    val and_ : bool -> bool -> bool

    val int32 : bool -> int32

    val pp : Format.formatter -> bool -> unit
  end

  module F32 : sig
    include
      Fop
        with type num := float32
         and type bool := bool
         and type int32 := int32
         and type int64 := int64
         and type same_size_int := int32

    val demote_f64 : float64 -> float32

    val reinterpret_i32 : int32 -> float32
  end

  module F64 : sig
    include
      Fop
        with type num := float64
         and type bool := bool
         and type int32 := int32
         and type int64 := int64
         and type same_size_int := int64

    val promote_f32 : float32 -> float64

    val reinterpret_i64 : int64 -> float64
  end

  module I32 : sig
    include
      Iop
        with type num := int32
         and type bool := bool
         and type const := Int32.t
         and type float32 := float32
         and type float64 := float64

    val to_bool : int32 -> bool

    val reinterpret_f32 : float32 -> int32

    val wrap_i64 : int64 -> int32
  end

  module I64 : sig
    include
      Iop
        with type num := int64
         and type bool := bool
         and type const := Int64.t
         and type float32 := float32
         and type float64 := float64

    val of_int32 : int32 -> int64

    val to_int32 : int64 -> int32

    val reinterpret_f64 : float64 -> int64

    val extend_i32_s : int32 -> int64

    val extend_i32_u : int32 -> int64
  end

  module V128 : sig
    val zero : v128

    val of_i32x4 : int32 -> int32 -> int32 -> int32 -> v128

    val to_i32x4 : v128 -> int32 * int32 * int32 * int32

    val of_i64x2 : int64 -> int64 -> v128

    val to_i64x2 : v128 -> int64 * int64
  end
end
