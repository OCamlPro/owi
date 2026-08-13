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

type array_obj = unit

type struct_obj = unit

type t =
  | Extern of Extern.t option
  | Func of int option
  | NullExn
  | NullRef
  | I31 of int32
  | NullI31
  | Array of array_obj
  | Struct of struct_obj

let pp fmt = function
  | Extern _ -> pf fmt "externref"
  | Func _ -> pf fmt "funcref"
  | NullExn -> pf fmt "nullexnref"
  | NullRef -> pf fmt "nullref"
  | I31 i -> pf fmt "i31ref %ld" i
  | NullI31 -> pf fmt "i31ref none"
  | Struct () -> pf fmt "structref"
  | Array () -> pf fmt "arrayref"

let null = function
  | Binary.Func_ht | NoFunc_ht | TypeUse _ -> Func None
  (* TODO: is this correct? Are all nulls equal? *)
  | Extern_ht | NoExtern_ht -> Extern None
  | Exn_ht | NoExn_ht -> NullExn
  | Any_ht | None_ht -> NullRef
  | Eq_ht | I31_ht -> NullI31
  | Struct_ht | Array_ht -> assert false

let func (f : int) = Func (Some f)

let extern (type x) (t : x Type.Id.t) (v : x) : t = Extern (Some (E (t, v)))

let make_i31 (n : int32) : t = I31 n

let is_null = function
  | Func None | Extern None | NullExn | NullRef | NullI31 -> true
  | Func (Some _) | Extern (Some _) | I31 _ | Array _ | Struct _ -> false

let get_func (r : t) : int get_ref =
  match r with
  | Func (Some f) -> Ref_value f
  | Func None -> Null
  | _ -> Type_mismatch

let get_i31 (r : t) : int32 get_ref =
  match r with
  | I31 n -> Ref_value n
  | NullI31 -> Null
  | _ -> Type_mismatch

let get_extern (type x) (r : t) (typ : x Type.Id.t) : x get_ref =
  match r with
  | Extern (Some (E (ety, v))) -> (
    match Type.Id.provably_equal typ ety with
    | None -> assert false
    | Some Equal -> Ref_value v )
  | _ -> assert false
