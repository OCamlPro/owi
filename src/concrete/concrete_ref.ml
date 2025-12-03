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

type t =
  | Extern of Extern.t option
  | Func of Kind.func option

let pp fmt = function
  | Extern _ -> pf fmt "externref"
  | Func _ -> pf fmt "funcref"

let null = function Text.Func_ht -> Func None | Extern_ht -> Extern None

let func (f : Kind.func) = Func (Some f)

let extern (type x) (t : x Type.Id.t) (v : x) : t = Extern (Some (E (t, v)))

let is_null = function
  | Func None | Extern None -> true
  | Func (Some _) | Extern (Some _) -> false

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
