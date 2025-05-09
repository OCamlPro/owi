(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Thread.Make (Symbolic_memory_concretizing)

let project (th : t) : Thread_without_memory.t * _ =
  let projected =
    let num_symbols = num_symbols th in
    let symbol_scopes = symbol_scopes th in
    let pc = pc th in
    let memories = Thread_without_memory.Memory.init () in
    let tables = tables th in
    let globals = globals th in
    let breadcrumbs = breadcrumbs th in
    let labels = labels th in
    Thread_without_memory.create num_symbols symbol_scopes pc memories tables
      globals breadcrumbs labels
  in
  let backup = memories th in
  (projected, backup)

let restore backup th =
  let num_symbols = Thread_without_memory.num_symbols th in
  let symbol_scopes = Thread_without_memory.symbol_scopes th in
  let pc = Thread_without_memory.pc th in
  let memories =
    if Thread_without_memory.memories th then
      Symbolic_memory_concretizing.clone backup
    else backup
  in
  let tables = Thread_without_memory.tables th in
  let globals = Thread_without_memory.globals th in
  let breadcrumbs = Thread_without_memory.breadcrumbs th in
  let labels = Thread_without_memory.labels th in
  create num_symbols symbol_scopes pc memories tables globals breadcrumbs labels
