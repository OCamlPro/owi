(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Symbolic_memory_concretizing.t

include
  Memory_intf.T
    with type t := t
     and type i32 := Symbolic_i32.t
     and type i64 := Symbolic_i64.t
     and type 'a choice := 'a Symbolic_choice_with_memory.t

type collection = Symbolic_memory_concretizing.collection

val init : unit -> collection

val clone : collection -> collection

val get_memory :
     int
  -> Concrete_memory.t
  -> collection
  -> int
  -> Symbolic_memory_concretizing.t

val realloc :
     Symbolic_memory_concretizing.t
  -> ptr:Smtml.Expr.t
  -> size:Smtml.Expr.t
  -> Smtml.Expr.t Symbolic_choice_without_memory.t

val free :
     Symbolic_memory_concretizing.t
  -> Smtml.Expr.t
  -> Smtml.Expr.t Symbolic_choice_without_memory.t
