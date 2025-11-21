(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'ext t

type elem = { mutable value : Concrete_value.ref_value array }

type data = { mutable value : string }

val get_memory : _ t -> int -> Concrete_memory.t Concrete_choice.t

val get_func : _ t -> int -> Kind.func

val get_table : _ t -> int -> Concrete_table.t Concrete_choice.t

val get_elem : _ t -> int -> elem

val get_data : _ t -> int -> data Concrete_choice.t

val get_global : _ t -> int -> Concrete_global.t Concrete_choice.t

val drop_elem : elem -> unit

val drop_data : data -> unit

val get_extern_func : 'ext t -> int -> 'ext

val id : _ t -> Env_id.t

module Build : sig
  type t

  val empty : t

  val add_global : int -> Concrete_global.t -> t -> t

  val add_memory : int -> Concrete_memory.t -> t -> t

  val add_table : int -> Concrete_table.t -> t -> t

  val add_func : int -> Kind.func -> t -> t

  val add_data : int -> data -> t -> t

  val add_elem : int -> elem -> t -> t

  val get_const_global : t -> int -> V.t Result.t

  val get_func : t -> int -> Kind.func Result.t
end

val freeze :
  Env_id.t -> Build.t -> ('ext * Binary.func_type) Dynarray.t -> 'ext t
