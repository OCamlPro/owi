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

  type boolean

  type i32

  module Array : Array_intf.T with type i32 = i32 and type boolean = boolean

  module Struct : Struct_intf.T with type boolean = boolean

  (* TODO; make this private and even opaque at some point *)
  type 'value t =
    | Extern of Extern.t option
    | Func of int option
    (* TODO: Not sure about these two. *)
    | NullExn
    | NullRef
    | I31 of i32
    | NullI31
    | Array of 'value Array.t
    | Struct of 'value Struct.t
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
end
