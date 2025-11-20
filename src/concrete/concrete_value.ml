(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type nonrec bool = bool

type int32 = Int32.t

let pp_int32 fmt i = Fmt.pf fmt "%ld" i

type int64 = Int64.t

let pp_int64 fmt i = Fmt.pf fmt "%Ld" i

type float32 = Float32.t

let pp_float32 = Float32.pp

type float64 = Float64.t

let pp_float64 = Float64.pp

type v128 = V128.t

let pp_v128 = V128.pp

let const_i32 x = x

let const_i64 x = x

let const_f32 x = x

let const_f64 x = x

let const_v128 x = x

include V

module Ref = struct
  let get_func (r : ref_value) : Kind.func Value_intf.get_ref =
    match r with
    | Funcref (Some f) -> Ref_value f
    | Funcref None -> Null
    | _ -> Type_mismatch

  let get_externref (type t) (r : ref_value) (t : t Type.Id.t) :
    t Value_intf.get_ref =
    match r with
    | Externref (Some (E (ety, v))) -> (
      match Type.Id.provably_equal t ety with
      | None -> assert false
      | Some Equal -> Ref_value v )
    | _ -> assert false
end

module Bool = struct
  let const c = c

  let not = not

  let and_ = ( && )

  let or_ = ( || )

  let int32 = function true -> 1l | false -> 0l

  let pp = Fmt.bool
end

module I32 = struct
  include Int32
  include Convert.Int32

  let to_bool i = Int32.ne i 0l

  let eq_const = eq
end

module I64 = struct
  include Int64
  include Convert.Int64

  let eq_const = eq
end

module F32 = struct
  include Float32
  include Convert.Float32
end

module F64 = struct
  include Float64
  include Convert.Float64
end

module V128 = struct
  include V128
end
