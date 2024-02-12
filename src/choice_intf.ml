(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module type Base = sig
  module V : Func_intf.Value_types

  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val select : V.vbool -> bool t

  val select_i32 : V.int32 -> Int32.t t

  val trap : Trap.t -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

module type Complete = sig
  include Base

  type thread

  type 'a run_result

  val assertion : V.vbool -> unit t

  val with_thread : (thread -> 'b) -> 'b t

  val solver : Solver.solver t

  val thread : thread t

  val add_pc : V.vbool -> unit t

  val run : workers:int -> 'a t -> thread -> 'a run_result
end
