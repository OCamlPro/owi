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
    ; labels : (int * string) list
    }

  let create symbols symbol_set pc memories tables globals breadcrumbs labels =
    { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs; labels }

  let init () =
    let symbols = 0 in
    let symbol_set = [] in
    let pc = Symbolic_path_condition.empty in
    let memories = Memory.init () in
    let tables = Symbolic_table.init () in
    let globals = Symbolic_global.init () in
    let breadcrumbs = [] in
    let labels = [] in
    create symbols symbol_set pc memories tables globals breadcrumbs labels

  let symbols t = t.symbols

  let symbols_set t = t.symbol_set

  let pc t = t.pc

  let memories t = t.memories

  let tables t = t.tables

  let globals t = t.globals

  let breadcrumbs t = t.breadcrumbs

  let labels t = t.labels

  let add_symbol t s =
    let symbol_set = s :: t.symbol_set in
    { t with symbol_set }

  let add_pc t c =
    let pc = Symbolic_path_condition.add t.pc c in
    { t with pc }

  let add_breadcrumb t crumb =
    let breadcrumbs = crumb :: t.breadcrumbs in
    { t with breadcrumbs }

  let incr_symbols t =
    let symbols = succ t.symbols in
    { t with symbols }

  let add_label t label = { t with labels = label :: t.labels }

  let clone
    { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs; labels }
      =
    let memories = Memory.clone memories in
    let tables = Symbolic_table.clone tables in
    let globals = Symbolic_global.clone globals in
    { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs; labels }
end
