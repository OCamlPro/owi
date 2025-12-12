(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module IMap : Map.S with type key = int

type 'ext t

val get_memory : _ t -> int -> Concrete_memory.t Concrete_choice.t

val get_func : _ t -> int -> Kind.func

val get_table : _ t -> int -> Concrete_table.t Concrete_choice.t

val get_elem : _ t -> int -> Concrete_elem.t

val get_data : _ t -> int -> Concrete_data.t Concrete_choice.t

val get_global : _ t -> int -> Concrete_global.t Concrete_choice.t

val get_extern_func : 'ext t -> int -> 'ext

val id : _ t -> int

module Build : sig
  type t

  val empty : t

  val add_global : int -> Concrete_global.t -> t -> t

  val add_memory : int -> Concrete_memory.t -> t -> t

  val add_table : int -> Concrete_table.t -> t -> t

  val add_func : int -> Kind.func -> t -> t

  val add_data : int -> Concrete_data.t -> t -> t

  val add_elem : int -> Concrete_elem.t -> t -> t

  val get_const_global : t -> int -> Concrete_value.t Result.t

  val get_func : t -> int -> Kind.func Result.t

  val get_memories : t -> Concrete_memory.t IMap.t
end

val freeze : int -> Build.t -> ('ext * Text.func_type) Dynarray.t -> 'ext t
