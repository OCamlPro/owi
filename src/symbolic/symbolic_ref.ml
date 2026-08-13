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
  | ExternAsAny of Extern.t option

let pp fmt = function
  | Extern _ -> pf fmt "externref"
  | Func _ -> pf fmt "funcref"
  | NullExn -> pf fmt "nullexnref"
  | NullRef -> pf fmt "nullref"
  | I31 i -> pf fmt "i31ref %ld" i
  | NullI31 -> pf fmt "i31ref none"
  | Struct () -> pf fmt "structref"
  | Array () -> pf fmt "arrayref"
  | ExternAsAny None -> pf fmt "anyref none"
  | ExternAsAny (Some _) -> pf fmt "anyref"

let null = function
  | Binary.Func_ht | NoFunc_ht | TypeUse _ -> Func None
  (* TODO: is this correct? Are all nulls equal? *)
  | Extern_ht | NoExtern_ht -> Extern None
  | Exn_ht | NoExn_ht -> NullExn
  | Any_ht | None_ht | Struct_ht | Array_ht -> NullRef
  | Eq_ht | I31_ht -> NullI31

let func (f : int) = Func (Some f)

let extern (type x) (t : x Type.Id.t) (v : x) : t = Extern (Some (E (t, v)))

let make_i31 (n : int32) : t = I31 n

let make_struct (_ : int) : t = Struct ()

let make_array (_ : int) : t = Array ()

let any_convert_extern = function
  | Extern None -> NullRef
  | Extern (Some e) -> ExternAsAny (Some e)
  | r -> ExternAsAny (Some (E (Type.Id.make (), r)))

let extern_convert_any = function
  | NullRef | NullI31 | NullExn | ExternAsAny None -> Extern None
  | ExternAsAny (Some e) -> Extern (Some e)
  | r -> Extern (Some (E (Type.Id.make (), r)))

let get_struct_type (_ : unit) : int option = None

let get_array_type (_ : unit) : int option = None

type gc_val = unit

let gc_val_of_view (_ : t Ref_intf.gc_view) : gc_val = ()

let view_gc_val (_ : gc_val) : t Ref_intf.gc_view =
  Fmt.failwith "TODO: unimplemented Symbolic_ref.view_gc_val"

let default_gc_val (_ : Binary.storage_type) : gc_val = ()

let struct_new_with (_ : int) (_ : gc_val array) : t = Struct ()

let struct_get_field (_ : struct_obj) (_ : int) : gc_val =
  Fmt.failwith "TODO: unimplemented Symbolic_ref.struct_get_field"

let struct_set_field (s : struct_obj) (_ : int) (_ : gc_val) : struct_obj = s

let array_new_fill (_ : int) (_ : gc_val) (_ : int) : t = Array ()

let array_new_fixed_with (_ : int) (_ : gc_val array) : t = Array ()

let array_get_elem (_ : array_obj) (_ : int) : gc_val =
  Fmt.failwith "TODO: unimplemented Symbolic_ref.array_get_elem"

let array_set_elem (a : array_obj) (_ : int) (_ : gc_val) : array_obj = a

let array_len_of (_ : array_obj) : int =
  Fmt.failwith "TODO: unimplemented Symbolic_ref.array_len_of"

let is_null = function
  | Func None | Extern None | NullExn | NullRef | NullI31 | ExternAsAny None ->
    true
  | Func (Some _)
  | Extern (Some _)
  | I31 _ | Array _ | Struct _
  | ExternAsAny (Some _) ->
    false

let get_func (r : t) : int get_ref =
  match r with
  | Func (Some f) -> Ref_value f
  | Func None -> Null
  | _ -> Type_mismatch

let get_i31 (r : t) : int32 get_ref =
  match r with I31 n -> Ref_value n | NullI31 -> Null | _ -> Type_mismatch

let get_extern (type x) (r : t) (typ : x Type.Id.t) : x get_ref =
  match r with
  | Extern (Some (E (ety, v))) -> (
    match Type.Id.provably_equal typ ety with
    | None -> assert false
    | Some Equal -> Ref_value v )
  | _ -> assert false
