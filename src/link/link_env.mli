(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type 'ext t

type 'ext backup

type t' = Env_id.t

type elem = { mutable value : Concrete_value.ref_value array }

type data = { mutable value : string }

type func := Func_intf.t

val backup : 'ext t -> 'ext backup

val recover : 'ext backup -> 'ext t -> unit

val get_memory : _ t -> int -> Concrete_memory.t Concrete_choice.t

val get_func : _ t -> int -> func

val get_table : _ t -> int -> Concrete_table.t Concrete_choice.t

val get_elem : _ t -> int -> elem

val get_data : _ t -> int -> data Concrete_choice.t

val get_global : _ t -> int -> Concrete_global.t Concrete_choice.t

val drop_elem : elem -> unit

val drop_data : data -> unit

val get_extern_func : 'ext t -> Func_id.t -> 'ext

val id : _ t -> Env_id.t

module Build : sig
  type t

  val empty : t

  val add_global : int -> Concrete_global.t -> t -> t

  val add_memory : int -> Concrete_memory.t -> t -> t

  val add_table : int -> Concrete_table.t -> t -> t

  val add_func : int -> func -> t -> t

  val add_data : int -> data -> t -> t

  val add_elem : int -> elem -> t -> t

  val get_const_global : t -> int -> V.t Result.t

  val get_func : t -> int -> func Result.t
end

type extern_funcs = Concrete_extern_func.extern_func Func_id.collection

val freeze : t' -> Build.t -> 'ext Func_id.collection -> 'ext t

module type T = sig
  type extern_func

  type t

  type elem = { mutable value : V.ref_value array }

  type data = { mutable value : string }

  val get_memory : t -> int -> Concrete_memory.t Result.t

  val get_func : t -> int -> func Result.t

  val get_table : t -> int -> Concrete_table.t Result.t

  val get_elem : t -> int -> elem Result.t

  val get_data : t -> int -> data Result.t

  val get_global : t -> int -> Concrete_global.t Result.t

  val drop_elem : elem -> unit

  val drop_data : data -> unit

  val get_extern_func : t -> Func_id.t -> Concrete_extern_func.extern_func

  val get_func_typ : t -> func -> func_type

  val pp : Format.formatter -> t -> unit

  val freeze : Build.t -> extern_func Func_id.collection -> t
end

module type P = sig
  val const_i32 : Int32.t -> Concrete_value.int32

  val const_i64 : Int64.t -> Concrete_value.int64

  val const_f32 : Float32.t -> Concrete_value.float32

  val const_f64 : Float64.t -> Concrete_value.float64
end
