(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** @inline *)
module Make (Thread : Thread_intf.S) :
  Symbolic_choice_intf.S
    with type 'a t = (('a, Bug.t) result, Thread.t) State_monad.t
     and type thread := Thread.t
