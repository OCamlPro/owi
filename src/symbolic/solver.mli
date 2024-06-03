(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Z3Batch : sig
  type t

  val create : ?params:Smtml.Params.t -> ?logic:Smtml.Ty.logic -> unit -> t

  val check : t -> Smtml.Expr.t list -> Smtml.Solver_intf.satisfiability

  val model : ?symbols:Smtml.Symbol.t list -> t -> Smtml.Model.t option
end

type t

val fresh : unit -> t

val check : t -> Smtml.Expr.t list -> Smtml.Solver_intf.satisfiability

val model : t -> symbols:Smtml.Symbol.t list -> Smtml.Model.t option
