(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** @inline *)
module Make (Symbolic_memory : Thread_intf.M) :
  Thread_intf.S with type Memory.collection = Symbolic_memory.collection
