(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = private
  { num_symbols : int
  ; symbol_scopes : Symbol_scope.t
  ; pc : Symbolic_path_condition.t
  ; memories : Symbolic_memory_collection.t
  ; tables : Symbolic_table_collection.t
  ; globals : Symbolic_global_collection.t
      (** Breadcrumbs represent the list of choices that were made so far. They
          identify one given symbolic execution trace. *)
  ; breadcrumbs : int list
  ; depth : int
  ; labels : (int * string) list
  ; bench_stats : Benchmark.stats
  }

val init : unit -> t

val create :
     int
  -> Symbol_scope.t
  -> Symbolic_path_condition.t
  -> Symbolic_memory_collection.t
  -> Symbolic_table_collection.t
  -> Symbolic_global_collection.t
  -> int list
  -> (int * string) list
  -> Benchmark.stats
  -> depth:int
  -> t

val clone : t -> t

val add_pc : t -> Symbolic_boolean.t -> t

val add_breadcrumb : t -> int -> t

val add_symbol : t -> Smtml.Symbol.t -> t

val add_label : t -> int * string -> t

val replace_memory :
  t -> env_id:int -> id:int -> Symbolic_memory_collection.memory -> t

val replace_table : Symbolic_table_collection.table -> t -> t

val replace_global : Symbolic_global_collection.global -> t -> t

val open_scope : t -> string -> t

val close_scope : t -> t

val incr_path_count : t -> unit

val incr_num_symbols : t -> t

val project : t -> t * Symbolic_memory_collection.t

val restore : Symbolic_memory_collection.t -> t -> t
