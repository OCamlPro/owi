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

  val pp : t Fmt.t

  val null : Binary.heap_type -> t

  val func : int -> t

  val extern : 'x Type.Id.t -> 'x -> t

  val make_i31 : int32 -> t

  val is_null : t -> Bool.t

  val get_func : t -> int get_ref

  val get_i31 : t -> int32 get_ref

  val get_extern : t -> 'x Type.Id.t -> 'x get_ref
end
