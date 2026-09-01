(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Choice_intf.S
    with type 'a t = ('a, Bug.t, Prio.metrics, Thread.t) Symex.Monad.t
     and type boolean := Symbolic_boolean.t
     and type i32 := Symbolic_i32.t
     and type value := Symbolic_value.t

val prune : unit -> 'a t

val assertion : Symbolic_boolean.t -> unit t

val map_state : (Thread.t -> Thread.t) -> unit t

val fold_state : (Thread.t -> 'a) -> 'a t

val with_new_invisible_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

val with_new_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'b) -> 'b t

val add_label : int * string -> unit t

val open_scope : string -> unit t

val close_scope : unit t
