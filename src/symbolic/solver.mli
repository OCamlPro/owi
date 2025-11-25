(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val fresh : Smtml.Solver_type.t -> unit -> t

val check : t -> Smtml.Expr.Set.t -> [ `Sat | `Unknown | `Unsat ]

val model :
  t -> symbol_scopes:Symbol_scope.t -> pc:Smtml.Expr.Set.t -> Smtml.Model.t

val empty_stats : Smtml.Statistics.t

val stats_are_empty : Smtml.Statistics.t -> bool

val pp_stats : Smtml.Statistics.t Fmt.t

val get_all_stats : finalizer:(unit -> unit) -> unit -> Smtml.Statistics.t
