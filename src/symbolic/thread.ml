(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)
include Thread_intf

module Make (Symbolic_memory : M) :
  S with type Memory.collection = Symbolic_memory.collection = struct
  module Memory : M with type collection = Symbolic_memory.collection =
    Symbolic_memory

  type t =
    { symbols : int
    ; symbol_set : Smtml.Symbol.t list
    ; pc : Symbolic_value.bool list
    ; pc_fresh : bool
    ; memories : Memory.collection
    ; tables : Symbolic_table.collection
    ; globals : Symbolic_global.collection
        (** Breadcrumbs represent the list of choices that were made so far.
            They identify one given symbolic execution trace. *)
    ; breadcrumbs : int32 list
    }

  let create symbols symbol_set pc pc_fresh memories tables globals breadcrumbs
      =
    { symbols
    ; symbol_set
    ; pc
    ; pc_fresh
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    }

  let init () =
    let symbols = 0 in
    let symbol_set = [] in
    let pc = [] in
    let pc_fresh = true in
    let memories = Memory.init () in
    let tables = Symbolic_table.init () in
    let globals = Symbolic_global.init () in
    let breadcrumbs = [] in
    create symbols symbol_set pc pc_fresh memories tables globals breadcrumbs

  let symbols t = t.symbols

  let symbols_set t = t.symbol_set

  let pc t = t.pc

  let pc_is_fresh t = t.pc_fresh

  let mark_pc_fresh t = { t with pc_fresh = true }

  let memories t = t.memories

  let tables t = t.tables

  let globals t = t.globals

  let breadcrumbs t = t.breadcrumbs

  let add_symbol t s = { t with symbol_set = s :: t.symbol_set }

  let add_pc t c = { t with pc = c :: t.pc; pc_fresh = false }

  let add_breadcrumb t crumb = { t with breadcrumbs = crumb :: t.breadcrumbs }

  let incr_symbols t = { t with symbols = succ t.symbols }

  let clone
    { symbols
    ; symbol_set
    ; pc
    ; pc_fresh
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    } =
    let memories = Memory.clone memories in
    let tables = Symbolic_table.clone tables in
    let globals = Symbolic_global.clone globals in
    { symbols
    ; symbol_set
    ; pc
    ; pc_fresh
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    }
end
