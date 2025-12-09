(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val fresh : Smtml.Solver_type.t -> unit -> t

val check : t -> Smtml.Expr.Set.t -> [ `Sat | `Unknown | `Unsat ]

val get_sat_model :
     t
  -> symbol_scopes:Symbol_scope.t
  -> pc:Smtml.Expr.Set.t
  -> [ `Unsat | `Unknown | `Model of Smtml.Model.t ]

val model_of_partition : t -> partition:Smtml.Expr.Set.t list -> Smtml.Model.t

val empty_stats : Smtml.Statistics.t

val stats_are_empty : Smtml.Statistics.t -> bool

val pp_stats : Smtml.Statistics.t Fmt.t

val get_all_stats : unit -> Smtml.Statistics.t

val interrupt_all : unit -> unit
