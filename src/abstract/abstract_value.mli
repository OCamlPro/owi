(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Size : sig
  val b32 : Units.In_bits.t

  val b64 : Units.In_bits.t

  val equal : Units.In_bits.t -> Units.In_bits.t -> bool
end

type boolean = Abstract_boolean.t

type i32 = Abstract_i32.t

type i64 = Abstract_i64.t

type f32 = Abstract_f32.t

type f64 = Abstract_f64.t

type v128 = Abstract_v128.t

type t =
  | I32 of i32
  | I64 of i64
  | F32 of f32
  | F64 of f64
  | V128 of v128
  | Ref of Abstract_ref.t

module Boolean : sig end

module I32 : sig
  val of_boolean : Abstract_domain.Context.t -> boolean -> i32

  val of_int32 : Abstract_domain.Context.t -> int32 -> i32

  val to_boolean : Abstract_domain.Context.t -> i32 -> boolean
end

module I64 : sig
  val of_int64 : Abstract_domain.Context.t -> int64 -> i64
end

module F32 : sig
  val of_float32 : Abstract_domain.Context.t -> Concrete_f32.t -> f32
end

module F64 : sig
  val of_float : Abstract_domain.Context.t -> float -> f64
end

module V128 : sig
  val of_concrete : Abstract_domain.Context.t -> Concrete_v128.t -> v128
end

module Ref : sig
  type t = Abstract_ref.t
end

val pp : t Fmt.t

val pp_with_ctx : Abstract_domain.Context.t -> t Fmt.t

val to_binary : t -> Abstract_domain.binary

val of_binary : Units.In_bits.t -> Abstract_domain.binary -> t

val size_of : t -> Units.In_bits.t

val to_boolean : Abstract_domain.Context.t -> t -> Abstract_boolean.t

val top : Units.In_bits.t -> Abstract_domain.Context.t -> t

val equal_script_result :
  Abstract_domain.Context.t -> ty:'a -> Wast.result -> t -> bool
