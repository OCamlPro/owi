(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Fmt

module Func =
  Func_intf.Make_extern_func
    (struct
      type int32 = Int32.t

      type int64 = Int64.t

      type float32 = Float32.t

      type float64 = Float64.t

      type nonrec bool = bool
    end)
    (struct
      type 'a t = 'a
    end)
    (Concrete_memory)

type externref = E : 'a Type.Id.t * 'a -> externref

let cast_ref (type r) (E (rty, r) : externref) (ty : r Type.Id.t) : r option =
  match Type.Id.provably_equal rty ty with None -> None | Some Equal -> Some r

type ref_value =
  | Externref of externref option
  | Funcref of Func_intf.t option
  | Arrayref of unit Array.t option

let pp_ref_value fmt = function
  | Externref _ -> pf fmt "externref"
  | Funcref _ -> pf fmt "funcref"
  | Arrayref _ -> pf fmt "array"

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of ref_value

(* TODO: make a new kind of instr for this *)
let of_instr (i : binary instr) : t =
  match i with
  | I32_const c -> I32 c
  | I64_const c -> I64 c
  | F32_const c -> F32 c
  | F64_const c -> F64 c
  | _ -> assert false

let to_instr = function
  | I32 c -> I32_const c
  | I64 c -> I64_const c
  | F32 c -> F32_const c
  | F64 c -> F64_const c
  | Ref _ -> assert false

let pp fmt = function
  | I32 i -> pf fmt "i32.const %ld" i
  | I64 i -> pf fmt "i64.const %Ld" i
  | F32 f -> pf fmt "f32.const %a" Float32.pp f
  | F64 f -> pf fmt "f64.const %a" Float64.pp f
  | Ref r -> pp_ref_value fmt r

let ref_null' = function
  | Func_ht -> Funcref None
  | Extern_ht -> Externref None
  | Array_ht -> Arrayref None
  | Any_ht | None_ht | Eq_ht | I31_ht | Struct_ht | No_func_ht | No_extern_ht
  | Def_ht _ ->
    assert false

let ref_null typ = Ref (ref_null' typ)

let ref_func (f : Func.t) : t = Ref (Funcref (Some f))

let ref_externref (type x) (t : x Type.Id.t) (v : x) : t =
  Ref (Externref (Some (E (t, v))))

let ref_is_null = function
  | Funcref None | Externref None | Arrayref None -> true
  | Funcref (Some _) | Externref (Some _) | Arrayref (Some _) -> false
