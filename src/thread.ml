(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type t =
  { choices : int
  ; mutable symbol_set : Encoding.Symbol.t list
  ; pc : Symbolic_value.S.vbool list
  ; memories : Symbolic_memory.memories
  ; tables : Symbolic_table.tables
  ; globals : Symbolic_global.globals
  }

let pc t = t.pc

let memories t = t.memories

let tables t = t.tables

let globals t = t.globals

let create () =
  { choices = 0
  ; symbol_set = []
  ; pc = []
  ; memories = Symbolic_memory.init ()
  ; tables = Symbolic_table.init ()
  ; globals = Symbolic_global.init ()
  }

let clone { choices; symbol_set; pc; memories; tables; globals } =
  let memories = Symbolic_memory.clone memories in
  let tables = Symbolic_table.clone tables in
  let globals = Symbolic_global.clone globals in
  { choices; symbol_set; pc; memories; tables; globals }
