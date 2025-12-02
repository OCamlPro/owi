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

  type i64

  type f32

  type f64

  type v128

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

  module V128 :
    V128_intf.T with type t := v128 and type i32 := i32 and type i64 := i64

  type t =
    | I32 of i32
    | I64 of i64
    | F32 of f32
    | F64 of f64
    | V128 of v128
    | Ref of Ref.t

  val pp : Format.formatter -> t -> unit
end
