(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type Parameters = sig
  val use_ite_for_select : bool

  val throw_away_trap : bool

  val timeout : float option

  val timeout_instr : int option

  val abstract_invariant : Abstract_invariant.t
end

module Default_parameters : Parameters

module Concrete (_ : Parameters) : sig
  val modul :
       env:Env.Concrete.t
    -> modul:Env.Concrete.modul
    -> Env.Concrete.t Concrete_choice.t

  val exec_vfunc_from_outside :
       env:Env.Concrete.t
    -> locals:Concrete_value.t list
    -> Concrete_extern.Func.t Kind.func
    -> (Env.Concrete.t * Concrete_value.t list) Concrete_choice.t
end

module Symbolic (_ : Parameters) : sig
  val modul :
       env:Env.Symbolic.t
    -> modul:Env.Symbolic.modul
    -> Env.Symbolic.t Symbolic_choice.t

  val exec_vfunc_from_outside :
       env:Env.Symbolic.t
    -> locals:Symbolic_value.t list
    -> Symbolic_extern.Func.t Kind.func
    -> (Env.Symbolic.t * Symbolic_value.t list) Symbolic_choice.t
end
