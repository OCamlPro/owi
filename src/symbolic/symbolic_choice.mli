(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = (('a, Bug.t) result, Thread.t) State_monad.t

val return : 'a -> 'a t

val stop : 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val trap : Result.err -> 'a t

val select :
     Symbolic_boolean.t
  -> prio_true:Prio.metrics
  -> prio_false:Prio.metrics
  -> Bool.t t

val select_i32 : Symbolic_i32.t -> Int32.t t

val assertion : Symbolic_boolean.t -> unit t

val assume : Symbolic_boolean.t -> Int.t Option.t -> unit t

val with_thread : (Thread.t -> 'a) -> 'a t

val modify_thread : (Thread.t -> Thread.t) -> unit t

val with_new_invisible_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

val with_new_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

val solver : unit -> Solver.t

val thread : Thread.t t

val get_pc : unit -> Smtml.Expr.Set.t t

val add_label : int * string -> unit t

val open_scope : string -> unit t

val close_scope : unit -> unit t

val run :
     Symbolic_parameters.Exploration_strategy.t
  -> workers:int
  -> Smtml.Solver_type.t
  -> 'a t
  -> Thread.t
  -> at_worker_value:
       (close_work_queue:(unit -> unit) -> ('a, Bug.t) result * Thread.t -> unit)
  -> at_worker_init:(unit -> unit)
  -> at_worker_end:(unit -> unit)
  -> unit Domain.t array

val ite :
     Symbolic_boolean.t
  -> if_true:Symbolic_value.t
  -> if_false:Symbolic_value.t
  -> Symbolic_value.t t

val depth : unit -> int t
