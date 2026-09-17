(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Choice_intf.S
    with type boolean := Concrete_boolean.t
     and type i32 := Concrete_i32.t
     and type value := Concrete_value.t

val map_state : (Concrete_state.t -> Concrete_state.t) -> unit t

val fold_state : (Concrete_state.t -> 'a) -> 'a t

val run :
     'a t
  -> Concrete_state.t
  -> (Concrete_state.t * 'a, Concrete_state.t * Result.err) Prelude.Result.t

val run_and_drop_state : 'a t -> Concrete_state.t -> 'a Result.t
