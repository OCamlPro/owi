(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type 'a get_ref =
    | Null
    | Ref_value of 'a
    | Type_mismatch

  module Extern : sig
    type t

    val cast : t -> 'x Type.Id.t -> 'x option
  end

  type 'value array_obj

  type 'value struct_obj

  type i32

  (* TODO; make this private and even opaque at some point *)
  type 'value t =
    | Extern of Extern.t option
    | Func of int option
    (* TODO: Not sure about these two. *)
    | NullExn
    | NullRef
    | I31 of i32
    | NullI31
    | Array of 'value array_obj
    | Struct of 'value struct_obj
    | ExternAsAny of Extern.t option
    | AnyAsExtern of 'value t

  val pp : 'value t Fmt.t

  val null : Binary.heap_type -> 'value t

  val func : int -> 'value t

  val extern : 'x Type.Id.t -> 'x -> 'value t

  val make_i31 : i32 -> 'value t

  val any_convert_extern : 'value t -> 'value t

  val extern_convert_any : 'value t -> 'value t

  val is_null : 'value t -> Bool.t

  val ref_eq : 'value t -> 'value t -> bool

  val get_func : 'value t -> int get_ref

  val get_i31 : 'value t -> i32 get_ref

  val get_extern : 'value t -> 'x Type.Id.t -> 'x get_ref

  val get_struct_type : 'value struct_obj -> int option

  val get_array_type : 'value array_obj -> int option

  (* TODO: calls to struct_set_field and array_set_elem from the interpreter are
     correct for the concrete case, in the symbolic case, we'll like want every
     execution branch to have its own instance of the heap, i.e. they should not
     be global, but local to every execution branch, so when a worker starts
     working on a branch, he gets all the information he needs on the living
     objects in that branch from the local heap instance. *)

  val struct_new_with : int -> 'value array -> 'value t

  val struct_get_field : 'value struct_obj -> int -> 'value

  val struct_set_field : 'value struct_obj -> int -> 'value -> unit

  val array_new_fill : int -> 'value -> int -> 'value t

  val array_new_fixed_with : int -> 'value array -> 'value t

  val array_get_elem : 'value array_obj -> int -> 'value

  val array_set_elem : 'value array_obj -> int -> 'value -> unit

  val array_len_of : 'value array_obj -> int
end
