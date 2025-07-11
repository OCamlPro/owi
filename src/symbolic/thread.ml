(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Make (Symbolic_memory : Thread_intf.M) :
  Thread_intf.S with type Memory.collection = Symbolic_memory.collection =
struct
  module Memory :
    Thread_intf.M with type collection = Symbolic_memory.collection =
    Symbolic_memory

  type t =
    { num_symbols : int
    ; symbol_scopes : Symbol_scope.t
    ; pc : Symbolic_path_condition.t
    ; pending_pc : Symbolic_value.bool list
    ; memories : Memory.collection
    ; tables : Symbolic_table.collection
    ; globals : Symbolic_global.collection
        (** Breadcrumbs represent the list of choices that were made so far.
            They identify one given symbolic execution trace. *)
    ; breadcrumbs : int list
    ; labels : (int * string) list
    }

  let create num_symbols symbol_scopes pc pending_pc memories tables globals
    breadcrumbs labels =
    { num_symbols
    ; symbol_scopes
    ; pc
    ; pending_pc
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    ; labels
    }

  let init () =
    let num_symbols = 0 in
    let symbol_scopes = Symbol_scope.empty in
    let pc = Symbolic_path_condition.empty in
    let pending_pc = [] in
    let memories = Memory.init () in
    let tables = Symbolic_table.init () in
    let globals = Symbolic_global.init () in
    let breadcrumbs = [] in
    let labels = [] in
    create num_symbols symbol_scopes pc pending_pc memories tables globals
      breadcrumbs labels

  let num_symbols t = t.num_symbols

  let symbol_scopes t = t.symbol_scopes

  let pc t = t.pc

  let pending_pc t = t.pending_pc

  let mark_pending_pc_done t = { t with pending_pc = [] }

  let memories t = t.memories

  let tables t = t.tables

  let globals t = t.globals

  let breadcrumbs t = t.breadcrumbs

  let labels t = t.labels

  let add_symbol t s =
    let open Symbol_scope in
    { t with symbol_scopes = symbol s t.symbol_scopes }

  let add_pc t c =
    let pc = Symbolic_path_condition.add t.pc c in
    { t with pc; pending_pc = c :: t.pending_pc }

  let add_breadcrumb t crumb =
    let breadcrumbs = crumb :: t.breadcrumbs in
    { t with breadcrumbs }

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

  let clone
    { num_symbols
    ; symbol_scopes
    ; pc
    ; pending_pc
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    ; labels
    } =
    if not @@ List.is_empty pending_pc then
      Fmt.epr "Warning: cloning thread with a non empty pending pc";
    let memories = Memory.clone memories in
    let tables = Symbolic_table.clone tables in
    let globals = Symbolic_global.clone globals in
    { num_symbols
    ; symbol_scopes
    ; pc
    ; pending_pc
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    ; labels
    }
end
