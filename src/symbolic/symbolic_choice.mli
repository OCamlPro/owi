(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = ('a, Bug.t, Prio.metrics, Thread.t) Symex.Monad.t

val return : 'a -> 'a t

val prune : unit -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val trap : Result.err -> 'a t

val select :
     Symbolic_boolean.t
  -> instr_counter_true:int option
  -> instr_counter_false:int option
  -> Bool.t t

val select_i32 : Symbolic_i32.t -> Int32.t t

val assertion : Symbolic_boolean.t -> unit t

val assume : Symbolic_boolean.t -> unit t

val modify_state : (Thread.t -> Thread.t) -> unit t

val with_new_invisible_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

val with_new_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

val solver : unit -> Solver.t

val solver_to_use : Smtml.Solver_type.t option ref

val state : Thread.t t

val get_pc : unit -> Smtml.Expr.Set.t t

val add_label : int * string -> unit t

val open_scope : string -> unit t

val close_scope : unit -> unit t

val ite :
     Symbolic_boolean.t
  -> if_true:Symbolic_value.t
  -> if_false:Symbolic_value.t
  -> Symbolic_value.t t
