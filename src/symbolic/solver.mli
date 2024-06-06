(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val fresh : Smtml.Solver_dispatcher.solver_type -> unit -> t

val check : t -> Smtml.Expr.t list -> Smtml.Solver_intf.satisfiability

val model :
     t
  -> symbols:Smtml.Symbol.t list option
  -> pc:Smtml.Expr.t list
  -> Smtml.Model.t
