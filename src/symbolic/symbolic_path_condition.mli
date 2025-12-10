(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val empty : t

val add : Symbolic_boolean.t -> t -> t

(* CAUTION: this must only be called after the symbol has been added to the path condition *)
val slice_on_symbol : Smtml.Symbol.t -> t -> Smtml.Expr.Set.t

(* CAUTION: this must only be called after the condition added to the path condition with `add` *)
val slice_on_condition : Symbolic_boolean.t -> t -> Smtml.Expr.Set.t

val slice : t -> Smtml.Expr.Set.t list
