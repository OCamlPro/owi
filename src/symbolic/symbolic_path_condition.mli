(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val empty : t

val add : t -> Symbolic_value.bool -> t

val slice : t -> Symbolic_value.bool -> Smtml.Expr.Set.t

val to_set : t -> Smtml.Expr.Set.t
