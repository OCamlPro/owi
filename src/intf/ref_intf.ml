(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: used for conversion, can it be removed? *)
type 'ref gc_view =
  | GCv_i32 of int32
  | GCv_i64 of int64
  | GCv_f32 of Float32.t
  | GCv_f64 of Float64.t
  | GCv_v128 of Concrete_v128.t
  | GCv_ref of 'ref

module type T = sig
  type 'a get_ref =
    | Null
    | Ref_value of 'a
    | Type_mismatch

  module Extern : sig
    type t

    val cast : t -> 'x Type.Id.t -> 'x option
  end

  type array_obj

  type struct_obj

  (* TODO; make this private and even opaque at some point *)
  type t =
    | Extern of Extern.t option
    | Func of int option
    (* TODO: Not sure about these two. *)
    | NullExn
    | NullRef
    | I31 of int32
    | NullI31
    | Array of array_obj
    | Struct of struct_obj
    | ExternAsAny of Extern.t option

  val pp : t Fmt.t

  val null : Binary.heap_type -> t

  val func : int -> t

  val extern : 'x Type.Id.t -> 'x -> t

  val make_i31 : int32 -> t

  val any_convert_extern : t -> t

  val extern_convert_any : t -> t

  val is_null : t -> Bool.t

  val ref_eq : t -> t -> bool

  val get_func : t -> int get_ref

  val get_i31 : t -> int32 get_ref

  val get_extern : t -> 'x Type.Id.t -> 'x get_ref

  val get_struct_type : struct_obj -> int option

  val get_array_type : array_obj -> int option

  (* TODO: calls to struct_set_field and array_set_elem from the interpreter are
     correct for the concrete case, in the symbolic case, we'll like want every
     execution branch to have its own instance of the heap, i.e. they should not
     be global, but local to every execution branch, so when a worker starts
     working on a branch, he gets all the information he needs on the living
     objects in that branch from the local heap instance. *)

  type gc_val

  val gc_val_of_view : t gc_view -> gc_val

  val view_gc_val : gc_val -> t gc_view

  val default_gc_val : Binary.storage_type -> gc_val

  val struct_new_with : int -> gc_val array -> t

  val struct_get_field : struct_obj -> int -> gc_val

  val struct_set_field : struct_obj -> int -> gc_val -> unit

  val array_new_fill : int -> gc_val -> int -> t

  val array_new_fixed_with : int -> gc_val array -> t

  val array_get_elem : array_obj -> int -> gc_val

  val array_set_elem : array_obj -> int -> gc_val -> unit

  val array_len_of : array_obj -> int
end
