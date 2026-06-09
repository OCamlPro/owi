(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: hide the definition completely to force proper propagation of values *)
type 'a t = private
  { raw : 'a
  ; instr_counter : int Atomic.t
  ; uuid : int
  ; mutable functions_called : Set.Make(Int).t
  ; d_true : int array option ref
  ; d_false : int array option ref
  }

val dummy : 'a -> 'a t

val dummies : 'a list -> 'a t list

val dummy_deep : 'a list -> 'a t list t

val map : ('a -> 'b) -> 'a t -> 'b t

val iter : ('a -> Unit.t) -> 'a t -> Unit.t

val update_functions_called : 'a t -> Set.Make(Int).t -> unit

val set_d_true : 'a t -> int array -> unit

val set_d_false : 'a t -> int array -> unit

val raw : 'a t -> 'a
