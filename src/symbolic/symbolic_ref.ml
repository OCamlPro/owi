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

type boolean = Symbolic_boolean.t

type i32 = Symbolic_i32.t

module Array = Symbolic_array
module Struct = Symbolic_struct

type 'value t =
  | Extern of Extern.t option
  | Func of int option
  | NullExn
  | NullRef
  | I31 of i32
  | NullI31
  | Array of 'value Array.t
  | Struct of 'value Struct.t
  | ExternAsAny of Extern.t option
  | AnyAsExtern of 'value t

let pp fmt = function
  | Extern _ -> pf fmt "externref"
  | Func _ -> pf fmt "funcref"
  | NullExn -> pf fmt "nullexnref"
  | NullRef -> pf fmt "nullref"
  | I31 i -> pf fmt "i31ref %a" Symbolic_i32.pp i
  | NullI31 -> pf fmt "i31ref none"
  | Struct _ -> pf fmt "structref"
  | Array _ -> pf fmt "arrayref"
  | ExternAsAny None -> pf fmt "anyref none"
  | ExternAsAny (Some _) -> pf fmt "anyref"
  | AnyAsExtern _ -> pf fmt "externref"

let null = function
  | Binary.Func_ht | NoFunc_ht | TypeUse _ -> Func None
  (* TODO: is this correct? Are all nulls equal? *)
  | Extern_ht | NoExtern_ht -> Extern None
  | Exn_ht | NoExn_ht -> NullExn
  | Any_ht | None_ht | Struct_ht | Array_ht -> NullRef
  | Eq_ht | I31_ht -> NullI31

let func (f : int) = Func (Some f)

let extern (type x) (t : x Type.Id.t) (v : x) : _ t = Extern (Some (E (t, v)))

let make_i31 (n : Symbolic_i32.t) : _ t = I31 n

let any_convert_extern : 'value t -> 'value t = function
  | Extern None -> NullRef
  | AnyAsExtern r -> r
  | Extern (Some e) -> ExternAsAny (Some e)
  | _ -> assert false

let extern_convert_any : 'value t -> 'value t = function
  | NullRef | NullI31 | NullExn | ExternAsAny None -> Extern None
  | ExternAsAny (Some e) -> Extern (Some e)
  | r -> AnyAsExtern r

let ref_eq (_ : 'value t) (_ : 'value t) : bool =
  Fmt.failwith "TODO: unimplemented Symbolic_ref.ref_eq"

let is_null = function
  | Func None | Extern None | NullExn | NullRef | NullI31 | ExternAsAny None ->
    true
  | Func (Some _)
  | Extern (Some _)
  | I31 _ | Array _ | Struct _
  | ExternAsAny (Some _)
  | AnyAsExtern _ ->
    false

let get_func (r : 'value t) : int get_ref =
  match r with
  | Func (Some f) -> Ref_value f
  | Func None -> Null
  | _ -> Type_mismatch

let get_i31 (r : 'value t) : i32 get_ref =
  match r with I31 n -> Ref_value n | NullI31 -> Null | _ -> Type_mismatch

let get_extern (type x) (r : 'value t) (typ : x Type.Id.t) : x get_ref =
  match r with
  | Extern (Some (E (ety, v))) -> (
    match Type.Id.provably_equal typ ety with
    | None -> assert false
    | Some Equal -> Ref_value v )
  | _ -> assert false
