(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Symbolic_memory_concretizing.t

include
  Memory_intf.T
    with module Value := Symbolic_value
     and module Choice := Symbolic_choice_with_memory
     and type t := t

type collection = Symbolic_memory_concretizing.collection

val init : unit -> collection

val clone : collection -> collection

val get_memory : int -> Concrete_memory.t -> collection -> int -> t

val realloc :
     t
  -> ptr:Smtml.Expr.t
  -> size:Smtml.Expr.t
  -> Smtml.Expr.t Symbolic_choice_without_memory.t

val free : t -> Smtml.Expr.t -> Smtml.Expr.t Symbolic_choice_without_memory.t
