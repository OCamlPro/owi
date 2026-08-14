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

and gc_obj =
  { obj_id : int32
  ; type_id : int
  ; fields : gc_val array
  }

and struct_obj = gc_obj

and array_obj = gc_obj

and t =
  | Extern of Extern.t option
  | Func of int option
  | NullExn
  | NullRef
  | I31 of int32
  | NullI31
  | Array of array_obj
  | Struct of struct_obj
  | ExternAsAny of Extern.t option

let any_as_extern_key : t Type.Id.t = Type.Id.make ()

(* Concrete execution is not parallel, so this should be fine *)
let obj_id_counter = ref 0l

let fresh_id () =
  let id = !obj_id_counter in
  obj_id_counter := Int32.add id 1l;
  id

let pp fmt = function
  | Extern None -> pf fmt "externref none"
  | Extern _ -> pf fmt "externref"
  | Func _ -> pf fmt "funcref"
  | NullExn -> pf fmt "nullexnref"
  | NullRef -> pf fmt "nullref"
  | I31 i -> pf fmt "i31ref %ld" i
  | NullI31 -> pf fmt "i31ref none"
  | Struct _ -> pf fmt "structref"
  | Array _ -> pf fmt "arrayref"
  | ExternAsAny None -> pf fmt "anyref none"
  | ExternAsAny (Some _) -> pf fmt "anyref"

(* TODO: Is this the same as Symbolic_ref.null? *)
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

let any_convert_extern = function
  | Extern None -> NullRef
  | Extern (Some (E (k, v))) -> (
    match Type.Id.provably_equal k any_as_extern_key with
    | Some Equal -> v
    | None -> ExternAsAny (Some (E (k, v))) )
  | r -> ExternAsAny (Some (E (any_as_extern_key, r)))

let extern_convert_any = function
  | NullRef | NullI31 | NullExn -> Extern None
  | ExternAsAny None -> Extern None
  | ExternAsAny (Some e) -> Extern (Some e)
  | r -> Extern (Some (E (any_as_extern_key, r)))

let is_null = function
  | Func None | Extern None | NullExn | NullRef | NullI31 | ExternAsAny None ->
    true
  | Func (Some _)
  | Extern (Some _)
  | I31 _ | Array _ | Struct _
  | ExternAsAny (Some _) ->
    false

let ref_eq (r1 : t) (r2 : t) : bool =
  if is_null r1 && is_null r2 then true
  else if is_null r1 || is_null r2 then false
  else
    match (r1, r2) with
    | I31 a, I31 b -> Int32.eq a b
    | Struct { obj_id = id1; _ }, Struct { obj_id = id2; _ } -> Int32.eq id1 id2
    | Array { obj_id = id1; _ }, Array { obj_id = id2; _ } -> Int32.eq id1 id2
    | _ -> false

let get_struct_type ({ type_id; _ } : struct_obj) = Some type_id

let get_array_type ({ type_id; _ } : array_obj) = Some type_id

let gc_val_of_view : t Ref_intf.gc_view -> gc_val = function
  | GCv_i32 i -> I32 i
  | GCv_i64 i -> I64 i
  | GCv_f32 f -> F32 f
  | GCv_f64 f -> F64 f
  | GCv_v128 v -> V128 v
  | GCv_ref r -> Ref r

let view_gc_val : gc_val -> t Ref_intf.gc_view = function
  | I32 i -> GCv_i32 i
  | I64 i -> GCv_i64 i
  | F32 f -> GCv_f32 f
  | F64 f -> GCv_f64 f
  | V128 v -> GCv_v128 v
  | Ref r -> GCv_ref r

let default_gc_val (st : Binary.storage_type) =
  match st with
  | Val_type (Num_type I32) -> I32 0l
  | Val_type (Num_type I64) -> I64 0L
  | Val_type (Num_type F32) -> F32 Float32.zero
  | Val_type (Num_type F64) -> F64 Float64.zero
  | Val_type (Num_type V128) -> V128 Concrete_v128.zero
  | Val_type (Ref_type (_, ht)) -> Ref (null ht)
  | Pack_type _ -> I32 0l

let struct_new_with type_id fields =
  Struct { obj_id = fresh_id (); type_id; fields }

let struct_get_field ({ fields; _ } : struct_obj) idx = fields.(idx)

let struct_set_field (s : struct_obj) idx v = s.fields.(idx) <- v

let array_new_fill type_id v n =
  Array { obj_id = fresh_id (); type_id; fields = Array.make n v }

let array_new_fixed_with type_id fields =
  Array { obj_id = fresh_id (); type_id; fields }

let array_get_elem ({ fields; _ } : array_obj) idx = fields.(idx)

let array_set_elem (a : array_obj) idx v = a.fields.(idx) <- v

let array_len_of ({ fields; _ } : array_obj) = Array.length fields

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
