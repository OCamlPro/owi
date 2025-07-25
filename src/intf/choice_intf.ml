(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Base = sig
  module V : Func_intf.Value_types

  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val select :
    V.bool -> prio_true:Prio.t -> prio_false:Prio.t -> Bool.t t

  val select_i32 : V.int32 -> Int32.t t

  val trap : Result.err -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val get_pc : unit -> Smtml.Expr.Set.t t
end

module type Complete = sig
  include Base

  type thread

  type 'a run_result

  val assertion : V.bool -> unit t

  val with_thread : (thread -> 'b) -> 'b t

  val solver : Solver.t t

  val thread : thread t

  val add_pc : V.bool -> unit t

  val lift_mem : 'a Symbolic_choice_without_memory.t -> 'a t
end
