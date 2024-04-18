(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { choices : int
  ; mutable symbol_set : Smtml.Symbol.t list
  ; pc : Symbolic_value.vbool list
  ; memories : Symbolic_memory.collection
  ; tables : Symbolic_table.collection
  ; globals : Symbolic_global.collection
      (** Breadcrumbs represent the list of choices that were made so far. They
          identify one given symbolic execution trace. *)
  ; breadcrumbs : int32 list
  }

let pc t = t.pc

let memories t = t.memories

let tables t = t.tables

let globals t = t.globals

let breadcrumbs t = t.breadcrumbs

let create () =
  { choices = 0
  ; symbol_set = []
  ; pc = []
  ; memories = Symbolic_memory.init ()
  ; tables = Symbolic_table.init ()
  ; globals = Symbolic_global.init ()
  ; breadcrumbs = []
  }

let clone { choices; symbol_set; pc; memories; tables; globals; breadcrumbs } =
  let memories = Symbolic_memory.clone memories in
  let tables = Symbolic_table.clone tables in
  let globals = Symbolic_global.clone globals in
  { choices; symbol_set; pc; memories; tables; globals; breadcrumbs }
