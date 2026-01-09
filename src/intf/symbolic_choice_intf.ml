(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a eval =
  | EVal of 'a
  | ETrap of
      Result.err
      * Smtml.Model.t
      * (int * string) list
      * int list
      * Symbol_scope.t
  | EAssert of
      Symbolic_boolean.t
      * Smtml.Model.t
      * (int * string) list
      * int list
      * Symbol_scope.t

module type S = sig
  type thread

  type 'a t

  val return : 'a -> 'a t

  val stop : 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val trap : Result.err -> 'a t

  val select :
    Symbolic_boolean.t -> prio_true:Prio.t -> prio_false:Prio.t -> Bool.t t

  val select_i32 : Symbolic_i32.t -> Int32.t t

  val assertion : Symbolic_boolean.t -> unit t

  val assume : Symbolic_boolean.t -> unit t

  val with_thread : (thread -> 'a) -> 'a t

  val with_new_invisible_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

  val with_new_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

  val solver : Solver.t t

  val thread : thread t

  val get_pc : unit -> Smtml.Expr.Set.t t

  val add_label : int * string -> unit t

  val open_scope : string -> unit t

  val close_scope : unit -> unit t

  type 'a run_result = ('a eval * thread) Seq.t

  val run :
       Symbolic_parameters.Exploration_strategy.t
    -> workers:int
    -> Smtml.Solver_type.t
    -> 'a t
    -> thread
    -> at_worker_value:
         (close_work_queue:(unit -> unit) -> 'a eval * thread -> unit)
    -> at_worker_init:(unit -> unit)
    -> at_worker_end:(unit -> unit)
    -> unit Domainpc.t array

  val ite :
       Symbolic_boolean.t
    -> if_true:Symbolic_value.t
    -> if_false:Symbolic_value.t
    -> Symbolic_value.t t
end

module type Intf = sig
  module type S = S

  module CoreImpl : sig
    (* The core implementation of the monad. It is isolated in a module to *)
    (* restict its exposed interface and maintain its invariant. *)

    module State : sig
      type ('a, 's) t

      val project_state :
           ('st1 -> 'st2 * 'backup)
        -> ('backup -> 'st2 -> 'st1)
        -> ('a, 'st2) t
        -> ('a, 'st1) t
    end
  end

  module Make (Thread : Thread_intf.S) :
    S
      with type 'a t = ('a eval, Thread.t) CoreImpl.State.t
       and type thread := Thread.t
end
