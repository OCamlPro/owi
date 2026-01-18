(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type S = sig
  type boolean

  type i32

  type value

  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val select :
    boolean -> prio_true:Prio.metrics -> prio_false:Prio.metrics -> Bool.t t

  val select_i32 : i32 -> Concrete_i32.t t

  val trap : Result.err -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val get_pc : unit -> Smtml.Expr.Set.t t

  val depth : unit -> int t

  val ite : boolean -> if_true:value -> if_false:value -> value t

  val assume : boolean -> Int.t Option.t -> unit t
end
