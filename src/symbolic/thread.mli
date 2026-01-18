(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val init : unit -> t

val create :
     int
  -> Symbol_scope.t
  -> Symbolic_path_condition.t
  -> Symbolic_memory_collection.collection
  -> Symbolic_table.collection
  -> Symbolic_global.collection
  -> int list
  -> (int * string) list
  -> Benchmark.stats
  -> depth:int
  -> t

val pc : t -> Symbolic_path_condition.t

val memories : t -> Symbolic_memory_collection.collection

val tables : t -> Symbolic_table.collection

val globals : t -> Symbolic_global.collection

val breadcrumbs : t -> int list

val depth : t -> int

val symbol_scopes : t -> Symbol_scope.t

val num_symbols : t -> int

val labels : t -> (int * string) list

val bench_stats : t -> Benchmark.stats

val clone : t -> t

val add_pc : t -> Symbolic_boolean.t -> t

val add_breadcrumb : t -> int -> t

val add_symbol : t -> Smtml.Symbol.t -> t

val add_label : t -> int * string -> t

val open_scope : t -> string -> t

val close_scope : t -> t

val incr_path_count : t -> unit

val incr_num_symbols : t -> t

val project : t -> t * Symbolic_memory_collection.collection

val restore : Symbolic_memory_collection.collection -> t -> t
