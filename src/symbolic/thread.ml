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
    { symbols : int
    ; symbol_set : Smtml.Symbol.t list
    ; pc : Symbolic_path_condition.t
    ; memories : Memory.collection
    ; tables : Symbolic_table.collection
    ; globals : Symbolic_global.collection
        (** Breadcrumbs represent the list of choices that were made so far.
            They identify one given symbolic execution trace. *)
    ; breadcrumbs : int32 list
    }

  let create symbols symbol_set pc memories tables globals breadcrumbs =
    { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs }

  let init () =
    let symbols = 0 in
    let symbol_set = [] in
    let pc = Symbolic_path_condition.empty in
    let memories = Memory.init () in
    let tables = Symbolic_table.init () in
    let globals = Symbolic_global.init () in
    let breadcrumbs = [] in
    create symbols symbol_set pc memories tables globals breadcrumbs

  let symbols t = t.symbols

  let symbols_set t = t.symbol_set

  let pc t = t.pc

  let memories t = t.memories

  let tables t = t.tables

  let globals t = t.globals

  let breadcrumbs t = t.breadcrumbs

  let add_symbol t s = { t with symbol_set = s :: t.symbol_set }

  let add_pc t c =
    let pc = Symbolic_path_condition.add t.pc c in
    { t with pc }

  let add_breadcrumb t crumb = { t with breadcrumbs = crumb :: t.breadcrumbs }

  let incr_symbols t = { t with symbols = succ t.symbols }

  let clone { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs }
      =
    let memories = Memory.clone memories in
    let tables = Symbolic_table.clone tables in
    let globals = Symbolic_global.clone globals in
    { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs }
end
