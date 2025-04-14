(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Thread.Make (Symbolic_memory_concretizing)

let project (th : t) : Thread_without_memory.t * _ =
  let projected =
    let symbols = symbols th in
    let symbols_set = symbols_set th in
    let pc = pc th in
    let memories = Thread_without_memory.Memory.init () in
    let tables = tables th in
    let globals = globals th in
    let breadcrumbs = breadcrumbs th in
    let labels = labels th in
    Thread_without_memory.create symbols symbols_set pc memories tables globals
      breadcrumbs labels
  in
  let backup = memories th in
  (projected, backup)

let restore backup th =
  let symbols = Thread_without_memory.symbols th in
  let symbols_set = Thread_without_memory.symbols_set th in
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
  create symbols symbols_set pc memories tables globals breadcrumbs labels
