(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

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

module Array = struct
  type i32 = Abstract_i32.t

  type boolean = Abstract_boolean.t

  type 'value t = |

  let get_type _a = assert false

  let new_fill _type_id _v _n = assert false

  let new_fixed_with _type_id _fields = assert false

  let get_elem _a _index = assert false

  let set_elem _a _index _v = assert false

  let length _a = assert false

  let phys_equal _a1 _a2 = assert false
end

module Struct = struct
  type boolean = Abstract_boolean.t

  type 'value t = |

  let new_with _type_id _fields = assert false

  let get_type _s = assert false

  let get_field _s = assert false

  let set_field _s _index _v = assert false

  let phys_equal _a1 _a2 = assert false
end

type i32

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
  | Extern _ -> Fmt.pf fmt "externref"
  | Func _ -> Fmt.pf fmt "funcref"
  | NullExn -> Fmt.pf fmt "nullexnref"
  | NullRef -> Fmt.pf fmt "nullref"
  | _ -> assert false

let null _ctx = function
  | Binary.Func_ht | NoFunc_ht | TypeUse _ -> Func None
  (* TODO: is this correct? Are all nulls equal? *)
  | Extern_ht | NoExtern_ht -> Extern None
  | Any_ht | None_ht | Exn_ht | NoExn_ht -> assert false
  | Eq_ht | I31_ht | Struct_ht | Array_ht -> assert false

let func (f : int) = Func (Some f)

let extern (type x) (t : x Type.Id.t) (v : x) : _ t = Extern (Some (E (t, v)))

let is_null = function
  | Func None | Extern None | NullExn | NullRef | NullI31 -> true
  | Func (Some _)
  | Extern (Some _)
  | I31 _ | Array _ | Struct _ | ExternAsAny _ | AnyAsExtern _ ->
    false

let get_func (r : _ t) : int get_ref =
  match r with
  | Func (Some f) -> Ref_value f
  | Func None -> Null
  | _ -> Type_mismatch

let get_extern (type x) (r : _ t) (typ : x Type.Id.t) : x get_ref =
  match r with
  | Extern (Some (E (ety, v))) -> (
    match Type.Id.provably_equal typ ety with
    | None -> assert false
    | Some Equal -> Ref_value v )
  | _ -> assert false
