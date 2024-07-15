(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a eval = private
  | EVal of 'a
  | ETrap of Trap.t * Smtml.Model.t
  | EAssert of Smtml.Expr.t * Smtml.Model.t

type 'a t = ('a, Thread_without_memory.t) Symbolic_choice.CoreImpl.Eval.t

type thread := Thread_without_memory.t

module V := Symbolic_value

type 'a run_result

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t

val select : V.vbool -> bool t

val select_i32 : V.int32 -> Int32.t t

val trap : Trap.t -> 'a t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val assertion : V.vbool -> unit t

val with_thread : (thread -> 'b) -> 'b t

val solver : Solver.t t

val thread : thread t

val add_pc : V.vbool -> unit t
