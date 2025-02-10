(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val fresh : Smtml.Solver_type.t -> unit -> t

val check : t -> Symbolic_path_condition.t -> [ `Sat | `Unknown | `Unsat ]

val model :
     t
  -> symbols:Smtml.Symbol.t list option
  -> pc:Symbolic_path_condition.t
  -> Smtml.Model.t
