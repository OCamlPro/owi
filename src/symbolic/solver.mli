(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val fresh : Smtml.Solver_type.t -> unit -> t

val check : t -> Smtml.Expr.Set.t -> [ `Sat | `Unknown | `Unsat ]

val model_of_set :
     t
  -> symbol_scopes:Symbol_scope.t
  -> set:Smtml.Expr.Set.t
  -> [ `Unsat | `Unknown | `Model of Smtml.Model.t ]

val model_of_path_condition :
  t -> path_condition:Symbolic_path_condition.t -> Smtml.Model.t option

val empty_stats : Smtml.Statistics.t

val stats_are_empty : Smtml.Statistics.t -> bool

val pp_stats : Smtml.Statistics.t Fmt.t

val get_all_stats :
  wait_for_all_domains:(Unit.t -> Unit.t) -> Smtml.Statistics.t
