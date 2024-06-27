(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val pc : t -> Symbolic_value.vbool list

val memories : t -> Symbolic_memory.collection

val tables : t -> Symbolic_table.collection

val globals : t -> Symbolic_global.collection

val breadcrumbs : t -> int32 list

val symbols_set : t -> Smtml.Symbol.t list

val symbols : t -> int

val create : unit -> t

val clone : t -> t

val add_pc : t -> Symbolic_value.vbool -> t

val add_breadcrumb : t -> int32 -> t

val add_symbol : t -> Smtml.Symbol.t -> t

val incr_symbols : t -> t
