(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Base = sig
  type boolean

  type i32

  type value

  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val select :
    boolean -> prio_true:Prio.source -> prio_false:Prio.source -> Bool.t t

  val select_i32 : i32 -> Concrete_i32.t t

  val trap : Result.err -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val get_pc : unit -> Smtml.Expr.Set.t t

  val depth : unit -> int t

  val ite : boolean -> if_true:value -> if_false:value -> value t

  val assume : boolean -> Int.t Option.t -> unit t
end

module type Complete = sig
  include Base

  type thread

  type 'a run_result

  val assertion : boolean -> unit t

  val assume : boolean -> unit t

  val with_thread : (thread -> 'b) -> 'b t

  val solver : Solver.t t

  val thread : thread t

  val lift_mem : 'a Symbolic_choice_without_memory.t -> 'a t
end
