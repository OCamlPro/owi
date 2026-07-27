(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
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

let init () =
  let num_symbols = 0 in
  let symbol_scopes = Symbol_scope.empty in
  let pc = Symex.Path_condition.empty in
  let breadcrumbs = [] in
  let labels = [] in
  let bench_stats = Benchmark.empty_stats () in
  let depth = 0 in
  let priority = Prio.dummy in
  { num_symbols
  ; symbol_scopes
  ; pc
  ; breadcrumbs
  ; labels
  ; bench_stats
  ; depth
  ; priority
  }

let add_symbol s t =
  let open Symbol_scope in
  let num_symbols = succ t.num_symbols in
  let symbol_scopes = symbol s t.symbol_scopes in
  { t with symbol_scopes; num_symbols }

let add_already_checked_condition_to_pc c t =
  let pc = Symex.Path_condition.add_checked_sat_condition c t.pc in
  { t with pc }

let add_breadcrumb crumb t =
  let breadcrumbs = crumb :: t.breadcrumbs in
  let depth = t.depth + 1 in
  Benchmark.set_max_depth t.bench_stats depth;
  { t with breadcrumbs; depth }

let incr_num_symbols t =
  let num_symbols = succ t.num_symbols in
  { t with num_symbols }

let add_label label t = { t with labels = label :: t.labels }

let open_scope scope t =
  let open Symbol_scope in
  { t with symbol_scopes = open_scope scope t.symbol_scopes }

let close_scope t =
  let open Symbol_scope in
  { t with symbol_scopes = close_scope t.symbol_scopes }

let incr_path_count t = Atomic.incr t.bench_stats.path_count

let set_priority priority thread = { thread with priority }
