(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type boolean

  type i32

  type f32

  type f64

  include
    Iop_intf.T
      with type t := t
       and type boolean := boolean
       and type concrete := Int64.t
       and type f32 := f32
       and type f64 := f64

  val of_int32 : i32 -> t

  val to_int32 : t -> i32

  val reinterpret_f64 : f64 -> t

  val extend_i32_s : i32 -> t

  val extend_i32_u : i32 -> t

  val of_concrete : Int64.t -> t

  val of_int : int -> t

  val pp : t Fmt.t
end
