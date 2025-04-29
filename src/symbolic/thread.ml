(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)
open Scoped_symbol

module Make (Symbolic_memory : Thread_intf.M) :
  Thread_intf.S with type Memory.collection = Symbolic_memory.collection =
struct
  module Memory :
    Thread_intf.M with type collection = Symbolic_memory.collection =
    Symbolic_memory

  type t =
    { num_symbols : int
    ; scoped_symbols : scope_token list
    ; pc : Symbolic_path_condition.t
    ; memories : Memory.collection
    ; tables : Symbolic_table.collection
    ; globals : Symbolic_global.collection
        (** Breadcrumbs represent the list of choices that were made so far.
            They identify one given symbolic execution trace. *)
    ; breadcrumbs : int list
    ; labels : (int * string) list
    }

  let create num_symbols scoped_symbols pc memories tables globals breadcrumbs
    labels =
    { num_symbols
    ; scoped_symbols
    ; pc
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    ; labels
    }

  let init () =
    let num_symbols = 0 in
    let scoped_symbols = [] in
    let pc = Symbolic_path_condition.empty in
    let memories = Memory.init () in
    let tables = Symbolic_table.init () in
    let globals = Symbolic_global.init () in
    let breadcrumbs = [] in
    let labels = [] in
    create num_symbols scoped_symbols pc memories tables globals breadcrumbs
      labels

  let num_symbols t = t.num_symbols

  let scoped_symbols t = t.scoped_symbols

  let pc t = t.pc

  let memories t = t.memories

  let tables t = t.tables

  let globals t = t.globals

  let breadcrumbs t = t.breadcrumbs

  let labels t = t.labels

  let add_symbol t s = { t with scoped_symbols = Symbol s :: t.scoped_symbols }

  let add_pc t c =
    let pc = Symbolic_path_condition.add t.pc c in
    { t with pc }

  let add_breadcrumb t crumb =
    let breadcrumbs = crumb :: t.breadcrumbs in
    { t with breadcrumbs }

  let incr_num_symbols t =
    let num_symbols = succ t.num_symbols in
    { t with num_symbols }

  let add_label t label = { t with labels = label :: t.labels }

  let open_scope t scope =
    { t with scoped_symbols = Open_scope scope :: t.scoped_symbols }

  let end_scope t = { t with scoped_symbols = Close_scope :: t.scoped_symbols }

  let clone
    { num_symbols
    ; scoped_symbols
    ; pc
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    ; labels
    } =
    let memories = Memory.clone memories in
    let tables = Symbolic_table.clone tables in
    let globals = Symbolic_global.clone globals in
    { num_symbols
    ; scoped_symbols
    ; pc
    ; memories
    ; tables
    ; globals
    ; breadcrumbs
    ; labels
    }
end
