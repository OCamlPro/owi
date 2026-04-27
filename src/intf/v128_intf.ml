(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type i8

  type i16

  type i32

  type i64

  val zero : t

  val of_i8x16 :
       i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> i8
    -> t

  val to_i8x16 :
       t
    -> i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8
       * i8

  val of_i16x8 : i16 -> i16 -> i16 -> i16 -> i16 -> i16 -> i16 -> i16 -> t

  val to_i16x8 : t -> i16 * i16 * i16 * i16 * i16 * i16 * i16 * i16

  val of_i32x4 : i32 -> i32 -> i32 -> i32 -> t

  val to_i32x4 : t -> i32 * i32 * i32 * i32

  val of_i64x2 : i64 -> i64 -> t

  val to_i64x2 : t -> i64 * i64

  val of_concrete : Concrete_v128.t -> t

  val pp : t Fmt.t
end
