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
    -> envs:Concrete_env.t Dynarray.t
    -> Kind.func
    -> V.t list Concrete_choice.t

  val exec_ibinop :
    V.t list -> Text.nn -> Text.ibinop -> V.t list Concrete_choice.t

  val exec_iunop : V.t list -> Text.nn -> Text.iunop -> V.t list

  val exec_itestop : V.t list -> Text.nn -> Text.itestop -> V.t list

  val exec_irelop : V.t list -> Text.nn -> Text.irelop -> V.t list

  val exec_itruncf :
    V.t list -> Text.nn -> Text.nn -> Text.sx -> V.t list Concrete_choice.t

  val exec_itruncsatf : V.t list -> Text.nn -> Text.nn -> Text.sx -> V.t list

  val exec_ireinterpretf : V.t list -> Text.nn -> Text.nn -> V.t list

  val exec_fbinop : V.t list -> Text.nn -> Text.fbinop -> V.t list

  val exec_funop : V.t list -> Text.nn -> Text.funop -> V.t list

  val exec_frelop : V.t list -> Text.nn -> Text.frelop -> V.t list

  val exec_fconverti : V.t list -> Text.nn -> Text.nn -> Text.sx -> V.t list

  val exec_freinterpreti : V.t list -> Text.nn -> Text.nn -> V.t list
end

module Symbolic : sig
  val modul :
       timeout:float option
    -> timeout_instr:int option
    -> Symbolic_extern_func.extern_func Link.State.t
    -> Symbolic_extern_func.extern_func Linked.Module.t
    -> unit Symbolic_choice_with_memory.t
end
