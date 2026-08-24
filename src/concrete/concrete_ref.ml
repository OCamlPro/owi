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

type i32 = Concrete_i32.t

type 'value gc_obj =
  { obj_id : int
  ; type_id : int
  ; fields : 'value array
  }

and 'value struct_obj = 'value gc_obj

and 'value array_obj = 'value gc_obj

and 'value t =
  | Extern of Extern.t option
  | Func of int option
  | NullExn
  | NullRef
  | I31 of i32
  | NullI31
  | Array of 'value array_obj
  | Struct of 'value struct_obj
  | ExternAsAny of Extern.t option
  | AnyAsExtern of 'value t

(* Concrete execution is not parallel, so this should be fine *)
let obj_id_counter = ref 0

let fresh_id () =
  let id = !obj_id_counter in
  incr obj_id_counter;
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
  | AnyAsExtern _ -> pf fmt "externref"

(* TODO: Is this the same as Symbolic_ref.null? *)
let null = function
  | Binary.Func_ht | NoFunc_ht | TypeUse _ -> Func None
  (* TODO: is this correct? Are all nulls equal? *)
  | Extern_ht | NoExtern_ht -> Extern None
  | Exn_ht | NoExn_ht -> NullExn
  | Any_ht | None_ht | Struct_ht | Array_ht -> NullRef
  | Eq_ht | I31_ht -> NullI31

let func (f : int) = Func (Some f)

let extern (type x) (t : x Type.Id.t) (v : x) : _ t = Extern (Some (E (t, v)))

let make_i31 (n : i32) : 'value t = I31 n

let any_convert_extern : 'value t -> 'value t = function
  | Extern None -> NullRef
  | AnyAsExtern r -> r
  | Extern (Some e) -> ExternAsAny (Some e)
  | _ -> assert false

let extern_convert_any : 'value t -> 'value t = function
  | NullRef | NullI31 | NullExn -> Extern None
  | ExternAsAny None -> Extern None
  | ExternAsAny (Some e) -> Extern (Some e)
  | r -> AnyAsExtern r

let is_null = function
  | Func None | Extern None | NullExn | NullRef | NullI31 | ExternAsAny None ->
    true
  | Func (Some _)
  | Extern (Some _)
  | I31 _ | Array _ | Struct _
  | ExternAsAny (Some _)
  | AnyAsExtern _ ->
    false

let rec ref_eq (r1 : 'value t) (r2 : 'value t) : bool =
  if is_null r1 || is_null r2 then is_null r1 && is_null r2
  else
    match (r1, r2) with
    | I31 a, I31 b -> Int32.eq a b
    | Struct { obj_id = id1; _ }, Struct { obj_id = id2; _ } -> id1 = id2
    | Array { obj_id = id1; _ }, Array { obj_id = id2; _ } -> id1 = id2
    | AnyAsExtern a, AnyAsExtern b -> ref_eq a b
    | _ -> false

let get_struct_type ({ type_id; _ } : 'value struct_obj) = Some type_id

let get_array_type ({ type_id; _ } : 'value array_obj) = Some type_id

let struct_new_with type_id fields =
  Struct { obj_id = fresh_id (); type_id; fields }

let struct_get_field ({ fields; _ } : 'value struct_obj) idx = fields.(idx)

let struct_set_field (s : 'value struct_obj) idx v = s.fields.(idx) <- v

let array_new_fill type_id v (n : Concrete_i32.t) =
  let obj_id = fresh_id () in
  let fields = Array.init (Concrete_i32.to_int n) (fun _i -> v) in
  Array { obj_id; type_id; fields }

let array_new_fixed_with type_id fields =
  Array { obj_id = fresh_id (); type_id; fields }

let array_get_elem ({ fields; _ } : 'value array_obj) idx = fields.(idx)

let array_set_elem (a : 'value array_obj) idx v = a.fields.(idx) <- v

let array_len_of ({ fields; _ } : 'value array_obj) = Array.length fields

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
