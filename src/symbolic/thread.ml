(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Collection = struct
  module Int_pair = struct
    type t = int * int

    let compare (l1, r1) (l2, r2) =
      let res = compare l1 l2 in
      if l1 = l2 then compare r1 r2 else res
  end

  module Int_pair_map = Map.Make (Int_pair)

  type 'a t = 'a Int_pair_map.t

  let empty = Int_pair_map.empty

  let find collection ~module_id ~id =
    let loc = (module_id, id) in
    Int_pair_map.find_opt loc collection

  let replace collection ~module_id ~id v =
    let loc = (module_id, id) in
    Int_pair_map.add loc v collection
end

type t =
  { num_symbols : int
  ; symbol_scopes : Symbol_scope.t
  ; pc : Symex.Path_condition.t
  ; memories : Symbolic_memory0.t Collection.t
  ; tables : Symbolic_table0.t Collection.t
  ; globals : Symbolic_global0.t Collection.t
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
  let memories = Collection.empty in
  let tables = Collection.empty in
  let globals = Collection.empty in
  let breadcrumbs = [] in
  let labels = [] in
  let bench_stats = Benchmark.empty_stats () in
  let depth = 0 in
  let priority = Prio.dummy in
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

let replace_memory (memory : Symbolic_memory0.t) thread =
  let memories = thread.memories in
  let memories =
    Collection.replace memories ~module_id:memory.module_id ~id:memory.id memory
  in
  { thread with memories }

let replace_table (table : Symbolic_table0.t) thread =
  let tables = thread.tables in
  let tables =
    Collection.replace tables ~module_id:table.module_id ~id:table.id table
  in
  { thread with tables }

let replace_global (global : Symbolic_global0.t) thread =
  let globals = thread.globals in
  let globals =
    Collection.replace globals ~module_id:global.module_id ~id:global.id global
  in
  { thread with globals }

let set_priority priority thread = { thread with priority }
