(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a eval = private
  | EVal of 'a
  | ETrap of Trap.t * Smtml.Model.t
  | EAssert of Smtml.Expr.t * Smtml.Model.t

include
  Choice_intf.Complete
    with type thread := Thread.t
     and type 'a run_result = ('a eval * Thread.t) Seq.t
     and module V := Symbolic_value

val run : workers:int -> 'a t -> Thread.t -> 'a run_result
