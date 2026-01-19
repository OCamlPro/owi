(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type memory =
  { mutable data : Symbolic_i32.t Map.Make(Int32).t
  ; mutable chunks : Symbolic_i32.t Map.Make(Int32).t
  ; mutable size : Symbolic_i32.t
  }

val memory_of_concrete : Concrete_memory.t -> memory

include Collection.S with type symbolic := memory
