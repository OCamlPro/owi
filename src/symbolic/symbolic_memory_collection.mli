(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type memory =
  { data : Symbolic_i32.t Map.Make(Int32).t
  ; chunks : Symbolic_i32.t Map.Make(Int32).t
  ; size : Symbolic_i32.t
  ; id : int * int
  }

type collection

val init : unit -> collection

val clone : collection -> collection

val get_memory : int -> Concrete_memory.t -> collection -> int -> memory

val set_memory : collection -> memory -> unit
