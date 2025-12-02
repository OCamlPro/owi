(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type boolean

  type f32

  type f64

  type i64

  include
    Iop_intf.T
      with type t := t
       and type boolean := boolean
       and type const := Int32.t
       and type f32 := f32
       and type f64 := f64

  val to_bool : t -> boolean

  val reinterpret_f32 : f32 -> t

  val wrap_i64 : i64 -> t
end
