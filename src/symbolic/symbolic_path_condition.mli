(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val empty : t

val add : t -> Symbolic_boolean.t -> t

(* CAUTION: this must only be called after `add` has been called! *)
val slice_on_symbol : t -> Smtml.Symbol.t -> Smtml.Expr.Set.t

(* CAUTION: this must only be called after `add` has been called! *)
val slice : t -> Symbolic_boolean.t -> Smtml.Expr.Set.t

val explode : t -> Smtml.Expr.Set.t list
