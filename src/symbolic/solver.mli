(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val solver_to_use : Smtml.Solver_type.t option ref

val check :
  Smtml.Expr.Set.t -> Smtml.Typed.Bool.t -> [ `Sat | `Unknown | `Unsat ]

val model_of_set :
     symbol_scopes:Symbol_scope.t
  -> set:Smtml.Expr.Set.t
  -> [ `Unsat | `Unknown | `Model of Smtml.Model.t ]

val model_of_path_condition :
  path_condition:Symex.Path_condition.t -> Smtml.Model.t option

val empty_stats : Smtml.Statistics.t

val stats_are_empty : Smtml.Statistics.t -> bool

val pp_stats : Smtml.Statistics.t Fmt.t

val get_all_stats :
  wait_for_all_domains:(Unit.t -> Unit.t) -> Smtml.Statistics.t

val was_interrupted : unit -> bool
