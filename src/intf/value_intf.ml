(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a get_ref =
  | Null
  | Ref_value of 'a
  | Type_mismatch

module type T = sig
  type boolean

  type i32

  val pp_int32 : Format.formatter -> i32 -> unit

  type i64

  val pp_int64 : Format.formatter -> i64 -> unit

  type f32

  val pp_float32 : Format.formatter -> f32 -> unit

  type f64

  val pp_float64 : Format.formatter -> f64 -> unit

  type v128

  val pp_v128 : Format.formatter -> v128 -> unit

  val const_i32 : Int32.t -> i32

  val const_i64 : Int64.t -> i64

  val const_f32 : Float32.t -> f32

  val const_f64 : Float64.t -> f64

  val const_v128 : V128.t -> v128

  module Boolean : Boolean_intf.T with type t := boolean and type i32 := i32

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
    | I32 of i32
    | I64 of i64
    | F32 of f32
    | F64 of f64
    | V128 of v128
    | Ref of Ref.t

  val ref_null : Text.heap_type -> t

  val ref_func : Kind.func -> t

  val ref_extern : 'x Type.Id.t -> 'x -> t

  val pp : Format.formatter -> t -> unit

  module F32 : sig
    include
      Fop_intf.T
        with type t := f32
         and type boolean := boolean
         and type i32 := i32
         and type i64 := i64
         and type same_size_int := i32

    val demote_f64 : f64 -> f32

    val reinterpret_i32 : i32 -> f32
  end

  module F64 : sig
    include
      Fop_intf.T
        with type t := f64
         and type boolean := boolean
         and type i32 := i32
         and type i64 := i64
         and type same_size_int := i64

    val promote_f32 : f32 -> f64

    val reinterpret_i64 : i64 -> f64
  end

  module I32 : sig
    include
      Iop_intf.T
        with type t := i32
         and type boolean := boolean
         and type const := Int32.t
         and type f32 := f32
         and type f64 := f64

    val to_bool : i32 -> boolean

    val reinterpret_f32 : f32 -> i32

    val wrap_i64 : i64 -> i32
  end

  module I64 : sig
    include
      Iop_intf.T
        with type t := i64
         and type boolean := boolean
         and type const := Int64.t
         and type f32 := f32
         and type f64 := f64

    val of_int32 : i32 -> i64

    val to_int32 : i64 -> i32

    val reinterpret_f64 : f64 -> i64

    val extend_i32_s : i32 -> i64

    val extend_i32_u : i32 -> i64
  end

  module V128 : sig
    val zero : v128

    val of_i32x4 : i32 -> i32 -> i32 -> i32 -> v128

    val to_i32x4 : v128 -> i32 * i32 * i32 * i32

    val of_i64x2 : i64 -> i64 -> v128

    val to_i64x2 : v128 -> i64 * i64
  end
end
