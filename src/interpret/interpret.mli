(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Parameters = sig
  val use_ite_for_select : bool

  val throw_away_trap : bool

  val timeout : float option

  val timeout_instr : int option
end

module Default_parameters : Parameters

module Concrete (_ : Parameters) : sig
  val modul :
       Concrete_extern_func.extern_func Link.State.t
    -> Concrete_extern_func.extern_func Linked.Module.t
    -> unit Concrete_choice.t

  val exec_vfunc_from_outside :
       locals:Concrete_value.t list
    -> env:int
    -> envs:Concrete_env.t Dynarray.t
    -> Kind.func
    -> Concrete_value.t list Concrete_choice.t

  val exec_ibinop :
       Concrete_value.t list
    -> Text.nn
    -> Text.ibinop
    -> Concrete_value.t list Concrete_choice.t

  val exec_iunop :
    Concrete_value.t list -> Text.nn -> Text.iunop -> Concrete_value.t list

  val exec_itestop :
    Concrete_value.t list -> Text.nn -> Text.itestop -> Concrete_value.t list

  val exec_irelop :
    Concrete_value.t list -> Text.nn -> Text.irelop -> Concrete_value.t list

  val exec_itruncf :
       Concrete_value.t list
    -> Text.nn
    -> Text.nn
    -> Text.sx
    -> Concrete_value.t list Concrete_choice.t

  val exec_itruncsatf :
       Concrete_value.t list
    -> Text.nn
    -> Text.nn
    -> Text.sx
    -> Concrete_value.t list

  val exec_ireinterpretf :
    Concrete_value.t list -> Text.nn -> Text.nn -> Concrete_value.t list

  val exec_fbinop :
    Concrete_value.t list -> Text.nn -> Text.fbinop -> Concrete_value.t list

  val exec_funop :
    Concrete_value.t list -> Text.nn -> Text.funop -> Concrete_value.t list

  val exec_frelop :
    Concrete_value.t list -> Text.nn -> Text.frelop -> Concrete_value.t list

  val exec_fconverti :
       Concrete_value.t list
    -> Text.nn
    -> Text.nn
    -> Text.sx
    -> Concrete_value.t list

  val exec_freinterpreti :
    Concrete_value.t list -> Text.nn -> Text.nn -> Concrete_value.t list
end

module Symbolic (_ : Parameters) : sig
  val modul :
       Symbolic_extern_func.extern_func Link.State.t
    -> Symbolic_extern_func.extern_func Linked.Module.t
    -> unit Symbolic_choice.t
end
