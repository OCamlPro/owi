(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Iop = sig
  type t

  type const

  type bool

  type float32

  type float64

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

  val shr_s : t -> t -> t

  val shr_u : t -> t -> t

  val rotl : t -> t -> t

  val rotr : t -> t -> t

  val eq_const : t -> const -> bool

  val eq : t -> t -> bool

  val ne : t -> t -> bool

  val lt : t -> t -> bool

  val gt : t -> t -> bool

  val lt_u : t -> t -> bool

  val gt_u : t -> t -> bool

  val le : t -> t -> bool

  val ge : t -> t -> bool

  val le_u : t -> t -> bool

  val ge_u : t -> t -> bool

  val trunc_f32_s : float32 -> t Result.t

  val trunc_f32_u : float32 -> t Result.t

  val trunc_f64_s : float64 -> t Result.t

  val trunc_f64_u : float64 -> t Result.t

  val trunc_sat_f32_s : float32 -> t

  val trunc_sat_f32_u : float32 -> t

  val trunc_sat_f64_s : float64 -> t

  val trunc_sat_f64_u : float64 -> t

  val extend_s : int -> t -> t
end

module type Fop = sig
  type t

  type bool

  type int32

  type int64

  type same_size_int

  val zero : t

  val abs : t -> t

  val neg : t -> t

  val sqrt : t -> t

  val ceil : t -> t

  val floor : t -> t

  val trunc : t -> t

  val nearest : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val min : t -> t -> t

  val max : t -> t -> t

  val copy_sign : t -> t -> t

  val eq : t -> t -> bool

  val ne : t -> t -> bool

  val lt : t -> t -> bool

  val gt : t -> t -> bool

  val le : t -> t -> bool

  val ge : t -> t -> bool

  val convert_i32_s : int32 -> t

  val convert_i32_u : int32 -> t

  val convert_i64_s : int64 -> t

  val convert_i64_u : int64 -> t

  val of_bits : same_size_int -> t

  val to_bits : t -> same_size_int
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

  val const_i32 : Int32.t -> int32

  val const_i64 : Int64.t -> int64

  val const_f32 : Float32.t -> float32

  val const_f64 : Float64.t -> float64

  val const_v128 : V128.t -> v128

  module Ref : sig
    module Extern : sig
      type t

      val cast : t -> 'x Type.Id.t -> 'x option
    end

    (* TODO; make this private and even opaque at some point *)
    type t =
      | Extern of Extern.t option
      | Func of Kind.func option

    val pp : Format.formatter -> t -> unit

    val null : Text.heap_type -> t

    val func : Kind.func -> t

    val extern : 'x Type.Id.t -> 'x -> t

    val is_null : t -> Bool.t

    val get_func : t -> Kind.func get_ref

    val get_extern : t -> 'x Type.Id.t -> 'x get_ref
  end

  type t =
    | I32 of int32
    | I64 of int64
    | F32 of float32
    | F64 of float64
    | V128 of v128
    | Ref of Ref.t

  val ref_null : Text.heap_type -> t

  val ref_func : Kind.func -> t

  val ref_extern : 'x Type.Id.t -> 'x -> t

  val pp : Format.formatter -> t -> unit

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
        with type t := float32
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
        with type t := float64
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
        with type t := int32
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
        with type t := int64
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
