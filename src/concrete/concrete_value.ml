(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

type boolean = bool

type i32 = Int32.t

type i64 = Int64.t

type f32 = Float32.t

type f64 = Float64.t

type v128 = V128.t

module Ref = struct
  type 'a get_ref =
    | Null
    | Ref_value of 'a
    | Type_mismatch

  module Extern = struct
    type t = E : 'a Type.Id.t * 'a -> t

    let cast (type r) (E (rty, r) : t) (ty : r Type.Id.t) : r option =
      match Type.Id.provably_equal rty ty with
      | None -> None
      | Some Equal -> Some r
  end

  type t =
    | Extern of Extern.t option
    | Func of Kind.func option

  let pp fmt = function
    | Extern _ -> pf fmt "externref"
    | Func _ -> pf fmt "funcref"

  let null = function Text.Func_ht -> Func None | Extern_ht -> Extern None

  let func (f : Kind.func) = Func (Some f)

  let extern (type x) (t : x Type.Id.t) (v : x) : t = Extern (Some (E (t, v)))

  let is_null = function
    | Func None | Extern None -> true
    | Func (Some _) | Extern (Some _) -> false

  let get_func (r : t) : Kind.func get_ref =
    match r with
    | Func (Some f) -> Ref_value f
    | Func None -> Null
    | _ -> Type_mismatch

  let get_extern (type x) (r : t) (typ : x Type.Id.t) : x get_ref =
    match r with
    | Extern (Some (E (ety, v))) -> (
      match Type.Id.provably_equal typ ety with
      | None -> assert false
      | Some Equal -> Ref_value v )
    | _ -> assert false
end

type t =
  | I32 of i32
  | I64 of i64
  | F32 of f32
  | F64 of f64
  | V128 of v128
  | Ref of Ref.t

let pp fmt = function
  | I32 i -> pf fmt "i32.const %ld" i
  | I64 i -> pf fmt "i64.const %Ld" i
  | F32 f -> pf fmt "f32.const %a" Float32.pp f
  | F64 f -> pf fmt "f64.const %a" Float64.pp f
  | V128 v -> pf fmt "v128.const %a" V128.pp v
  | Ref r -> Ref.pp fmt r

module Boolean = struct
  let false_ = false

  let true_ = true

  let of_concrete c = c

  let not = not

  let and_ = ( && )

  let or_ = ( || )

  let to_i32 = function false -> 0l | true -> 1l

  let pp = Fmt.bool
end

module I32 = struct
  include Int32
  include Convert.Int32

  let to_bool = function 0l -> false | _i -> true

  let of_concrete v = v

  let eq_concrete = eq

  let pp = Fmt.int32
end

module I64 = struct
  include Int64
  include Convert.Int64

  let of_concrete v = v

  let eq_concrete = eq

  let pp = Fmt.int64
end

module F32 = struct
  include Float32
  include Convert.Float32

  let of_concrete v = v
end

module F64 = struct
  include Float64
  include Convert.Float64

  let of_concrete v = v
end

module V128 = struct
  include V128

  let of_concrete v = v
end
