(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to define externref values in OCaml. You should look in the `example`
    directory to understand how to use this before reading the code... *)

type externref = E : 'a Type.Id.t * 'a -> externref

type ref_value =
  | Externref of externref option
  | Funcref of Func_intf.t option

val pp_ref_value : Format.formatter -> ref_value -> unit

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | V128 of V128.t
  | Ref of ref_value

val cast_ref : externref -> 'a Type.Id.t -> 'a option

val of_instr : Binary.instr -> t

val to_instr : t -> Binary.instr

val ref_null' : Binary.heap_type -> ref_value

val ref_null : Binary.heap_type -> t

val ref_func : Concrete_extern_func.t -> t

val ref_externref : 'a Type.Id.t -> 'a -> t

val ref_is_null : ref_value -> Bool.t

val pp : Format.formatter -> t -> unit
