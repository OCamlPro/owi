(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type i32

  type i64

  type f32

  type f64

  type v128

  type boolean

  module Boolean : Boolean_intf.T with type t := boolean

  module I32 :
    I32_intf.T
      with type t := i32
       and type boolean := boolean
       and type f32 := f32
       and type f64 := f64
       and type i64 := i64

  module I64 :
    I64_intf.T
      with type t := i64
       and type boolean := boolean
       and type i32 := i32
       and type f32 := f32
       and type f64 := f64

  module F32 :
    F32_intf.T
      with type t := f32
       and type boolean := boolean
       and type i32 := i32
       and type i64 := i64
       and type f64 := f64

  module F64 :
    F64_intf.T
      with type t := f64
       and type boolean := boolean
       and type f32 := f32
       and type i32 := i32
       and type i64 := i64

  module V128 :
    V128_intf.T with type t := v128 and type i32 := i32 and type i64 := i64

  module Ref : Ref_intf.T

  type t =
    | I32 of i32
    | I64 of i64
    | F32 of f32
    | F64 of f64
    | V128 of v128
    | Ref of Ref.t

  val pp : t Fmt.t
end
