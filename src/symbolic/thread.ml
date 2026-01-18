(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { num_symbols : int
  ; symbol_scopes : Symbol_scope.t
  ; pc : Symbolic_path_condition.t
  ; memories : Symbolic_memory_collection.collection
  ; tables : Symbolic_table.collection
  ; globals : Symbolic_global.collection
      (** Breadcrumbs represent the list of choices that were made so far. They
          identify one given symbolic execution trace. *)
  ; breadcrumbs : int list
  ; depth : int
  ; labels : (int * string) list
  ; bench_stats : Benchmark.stats
  }

let create num_symbols symbol_scopes pc memories tables globals breadcrumbs
  labels bench_stats ~depth =
  { num_symbols
  ; symbol_scopes
  ; pc
  ; memories
  ; tables
  ; globals
  ; breadcrumbs
  ; labels
  ; bench_stats
  ; depth
  }

let init () =
  let num_symbols = 0 in
  let symbol_scopes = Symbol_scope.empty in
  let pc = Symbolic_path_condition.empty in
  let memories = Symbolic_memory_collection.init () in
  let tables = Symbolic_table.init () in
  let globals = Symbolic_global.init () in
  let breadcrumbs = [] in
  let labels = [] in
  let bench_stats = Benchmark.empty_stats () in
  let depth = 0 in
  create num_symbols symbol_scopes pc memories tables globals breadcrumbs labels
    bench_stats ~depth

let num_symbols t = t.num_symbols

let symbol_scopes t = t.symbol_scopes

let pc t = t.pc

let memories t = t.memories

let tables t = t.tables

let globals t = t.globals

let breadcrumbs t = t.breadcrumbs

let depth t = t.depth

let labels t = t.labels

let bench_stats t = t.bench_stats

let add_symbol t s =
  let open Symbol_scope in
  { t with symbol_scopes = symbol s t.symbol_scopes }

let add_pc t c =
  let pc = Symbolic_path_condition.add c t.pc in
  { t with pc }

let add_breadcrumb t crumb =
  let breadcrumbs = crumb :: t.breadcrumbs in
  let depth = t.depth + 1 in
  { t with breadcrumbs; depth }

let incr_num_symbols t =
  let num_symbols = succ t.num_symbols in
  { t with num_symbols }

let add_label t label = { t with labels = label :: t.labels }

let open_scope t scope =
  let open Symbol_scope in
  { t with symbol_scopes = open_scope scope t.symbol_scopes }

let close_scope t =
  let open Symbol_scope in
  { t with symbol_scopes = close_scope t.symbol_scopes }

let incr_path_count t = Atomic.incr (bench_stats t).path_count

let clone
  { num_symbols
  ; symbol_scopes
  ; pc
  ; memories
  ; tables
  ; globals
  ; breadcrumbs
  ; labels
  ; bench_stats
  ; depth
  } =
  (* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
  let memories = Symbolic_memory_collection.clone memories in
  let tables = Symbolic_table.clone tables in
  let globals = Symbolic_global.clone globals in
  { num_symbols
  ; symbol_scopes
  ; pc
  ; memories
  ; tables
  ; globals
  ; breadcrumbs
  ; labels
  ; bench_stats
  ; depth
  }

let project (th : t) : t * _ =
  let projected =
    let num_symbols = num_symbols th in
    let symbol_scopes = symbol_scopes th in
    let pc = pc th in
    let memories = Symbolic_memory_collection.init () in
    let tables = tables th in
    let globals = globals th in
    let breadcrumbs = breadcrumbs th in
    let labels = labels th in
    let bench_stats = bench_stats th in
    let depth = depth th in
    create num_symbols symbol_scopes pc memories tables globals breadcrumbs
      labels bench_stats ~depth
  in
  let backup = memories th in
  (projected, backup)

let restore backup th =
  let num_symbols = num_symbols th in
  let symbol_scopes = symbol_scopes th in
  let pc = pc th in
  let memories =
    if true then Symbolic_memory_collection.clone backup else backup
  in
  let tables = tables th in
  let globals = globals th in
  let breadcrumbs = breadcrumbs th in
  let labels = labels th in
  let bench_stats = bench_stats th in
  let depth = depth th in
  create num_symbols symbol_scopes pc memories tables globals breadcrumbs labels
    bench_stats ~depth
