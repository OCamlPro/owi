(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a eval =
  | EVal of 'a
  | ETrap of Trap.t * Smtml.Model.t
  | EAssert of Smtml.Expr.t * Smtml.Model.t

module type S = sig
  module V : Func_intf.Value_types

  type thread

  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val trap : Trap.t -> 'a t

  val select : V.vbool -> bool t

  val select_i32 : V.int32 -> Int32.t t

  val assertion : V.vbool -> unit t

  val with_thread : (thread -> 'a) -> 'a t

  val with_new_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

  val solver : Solver.t t

  val thread : thread t

  val add_pc : V.vbool -> unit t

  type 'a run_result = ('a eval * thread) Seq.t

  val run :
       workers:int
    -> Smtml.Solver_type.t
    -> 'a t
    -> thread
    -> callback:('a eval * thread -> unit)
    -> callback_init:(unit -> unit)
    -> callback_end:(unit -> unit)
    -> unit Domain.t array
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

  module Make (Thread : Thread.S) :
    S
      with type 'a t = ('a eval, Thread.t) CoreImpl.State.t
       and type thread := Thread.t
       and module V := Symbolic_value
end
