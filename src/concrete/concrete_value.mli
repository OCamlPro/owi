(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to define externref values in OCaml. You should look in the `example`
    directory to understand how to use this before reading the code... *)

open Types

type externref = E : 'a Type.Id.t * 'a -> externref

module Func :
  Func_intf.T
    with type int32 := Int32.t
     and type int64 := Int64.t
     and type float32 := Float32.t
     and type float64 := Float64.t
     and type 'a m := 'a
     and type memory := Concrete_memory.t

type ref_value =
  | Externref of externref option
  | Funcref of Func_intf.t option
  | Arrayref of unit array option

val pp_ref_value : Format.formatter -> ref_value -> unit

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of ref_value

val cast_ref : externref -> 'a Type.Id.t -> 'a option

val of_instr : binary instr -> t

val to_instr : t -> binary instr

val ref_null' : binary heap_type -> ref_value

val ref_null : binary heap_type -> t

val ref_func : Func.t -> t

val ref_externref : 'a Type.Id.t -> 'a -> t

val ref_is_null : ref_value -> Bool.t

val pp : Format.formatter -> t -> unit
