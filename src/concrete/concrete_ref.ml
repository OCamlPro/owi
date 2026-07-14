(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

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

type gc_val =
  | I32 of int32
  | I64 of int64
  | F32 of Float32.t
  | F64 of Float64.t
  | V128 of Concrete_v128.t
  | Ref of t

and struct_obj = gc_val array

and array_obj = gc_val array

and t =
  | Extern of Extern.t option
  | Func of Kind.func option
  | NullExn
  | NullRef
  | I31 of int32
  | Array of array_obj
  | Struct of struct_obj

let any_as_extern_key : t Type.Id.t = Type.Id.make ()

let pp fmt = function
  | Extern None -> pf fmt "externref none"
  | Extern _ -> pf fmt "externref"
  | Func _ -> pf fmt "funcref"
  | NullExn -> pf fmt "nullexnref"
  | NullRef -> pf fmt "nullref"
  | I31 i -> pf fmt "i31ref %ld" i
  | Struct _ -> pf fmt "structref"
  | Array _ -> pf fmt "arrayref"

(* TODO: Is this the same as Symbolic_ref.null? *)
let null = function
  | Binary.Func_ht | NoFunc_ht | TypeUse _ -> Func None
  (* TODO: is this correct? Are all nulls equal? *)
  | Extern_ht | NoExtern_ht -> Extern None
  | Exn_ht | NoExn_ht -> NullExn
  | Any_ht | None_ht | Eq_ht | I31_ht | Struct_ht | Array_ht -> NullRef

let func (f : Kind.func) = Func (Some f)

let extern (type x) (t : x Type.Id.t) (v : x) : t = Extern (Some (E (t, v)))

let is_null = function
  | Func None | Extern None | NullExn | NullRef -> true
  | Func (Some _) | Extern (Some _) | I31 _ | Array _ | Struct _ -> false

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
