(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type err = private
  | Assert_fail
  | Trap of Trap.t

include
  Choice_intf.Complete
    with type thread := Thread.t
     and type 'a run_result = ('a, err) Stdlib.Result.t * Thread.t
     and module V := Symbolic_value
