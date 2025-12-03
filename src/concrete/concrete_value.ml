(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

type i32 = Int32.t

type i64 = Int64.t

type f32 = Float32.t

type f64 = Float64.t

type v128 = V128.t

module Boolean = Concrete_boolean
module Ref = Concrete_ref

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
