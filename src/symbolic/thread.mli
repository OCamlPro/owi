(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = private
  { num_symbols : int
  ; symbol_scopes : Symbol_scope.t
  ; pc : Symex.Path_condition.t
      (** Breadcrumbs represent the list of choices that were made so far. They
          identify one given symbolic execution trace. *)
  ; breadcrumbs : int list
  ; depth : int
  ; labels : (int * string) list
  ; bench_stats : Benchmark.stats
  ; priority : Prio.metrics
  }

val init : unit -> t

val add_already_checked_condition_to_pc : Symbolic_boolean.t -> t -> t

val add_breadcrumb : int -> t -> t

val add_symbol : Smtml.Symbol.t -> t -> t

val add_label : int * string -> t -> t

val open_scope : string -> t -> t

val close_scope : t -> t

val incr_path_count : t -> unit

val incr_num_symbols : t -> t

val set_priority : Prio.metrics -> t -> t
