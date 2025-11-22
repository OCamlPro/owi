(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val unset_use_ite_for_select : unit -> unit

module Concrete : sig
  val modul :
       timeout:float option
    -> timeout_instr:int option
    -> Concrete_extern_func.extern_func Link.State.t
    -> Concrete_extern_func.extern_func Linked.Module.t
    -> unit Concrete_choice.t

  val exec_vfunc_from_outside :
       locals:Concrete_value.t list
    -> env:int
    -> envs:Concrete.Env.t Dynarray.t
    -> Kind.func
    -> V.t list Concrete_choice.t

  val exec_ibinop :
    V.t list -> Binary.nn -> Binary.ibinop -> V.t list Concrete_choice.t

  val exec_iunop : V.t list -> Binary.nn -> Binary.iunop -> V.t list

  val exec_itestop : V.t list -> Binary.nn -> Binary.itestop -> V.t list

  val exec_irelop : V.t list -> Binary.nn -> Binary.irelop -> V.t list

  val exec_itruncf :
       V.t list
    -> Binary.nn
    -> Binary.nn
    -> Binary.sx
    -> V.t list Concrete_choice.t

  val exec_itruncsatf :
    V.t list -> Binary.nn -> Binary.nn -> Binary.sx -> V.t list

  val exec_ireinterpretf : V.t list -> Binary.nn -> Binary.nn -> V.t list

  val exec_fbinop : V.t list -> Binary.nn -> Binary.fbinop -> V.t list

  val exec_funop : V.t list -> Binary.nn -> Binary.funop -> V.t list

  val exec_frelop : V.t list -> Binary.nn -> Binary.frelop -> V.t list

  val exec_fconverti :
    V.t list -> Binary.nn -> Binary.nn -> Binary.sx -> V.t list

  val exec_freinterpreti : V.t list -> Binary.nn -> Binary.nn -> V.t list
end

module Symbolic : sig
  val modul :
       timeout:float option
    -> timeout_instr:int option
    -> Symbolic.Extern_func.extern_func Link.State.t
    -> Symbolic.Extern_func.extern_func Linked.Module.t
    -> unit Symbolic.Choice.t
end
