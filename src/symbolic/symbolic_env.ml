(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Link.Symbolic.State.t

let get_memory ~(modul : int) env id : Symbolic_memory.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let* memories = Symbolic_choice.fold_state (fun state -> state.memories) in
  match Thread.Collection.find memories ~module_id:modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    let original = Link.Symbolic.State.get_memory env ~modul id in
    let symbolic = Symbolic_memory.of_concrete ~module_id:modul ~id original in
    let* () = Symbolic_memory.replace symbolic in
    Symbolic_choice.return symbolic

let get_table ~(modul : int) env id : Symbolic_table.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let* tables = Symbolic_choice.fold_state (fun state -> state.tables) in
  match Thread.Collection.find tables ~module_id:modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    let original = Link.Symbolic.State.get_table env ~modul id in
    let symbolic = Symbolic_table.of_concrete ~module_id:modul ~id original in
    let* () = Symbolic_table.replace symbolic in
    Symbolic_choice.return symbolic

let get_data ~(modul : int) env n = Link.Symbolic.State.get_data ~modul env n

let get_global ~(modul : int) env id : Symbolic_global.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let* globals = Symbolic_choice.fold_state (fun state -> state.globals) in
  match Thread.Collection.find globals ~module_id:modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    let original = Link.Symbolic.State.get_global env ~modul id in
    let symbolic = Symbolic_global.of_concrete ~module_id:modul ~id original in
    let* () = Symbolic_global.replace symbolic in
    Symbolic_choice.return symbolic

let get_func ~modul env id = Link.Symbolic.State.get_func env ~modul id

let get_elem ~modul env id = Link.Symbolic.State.get_elem env ~modul id

let get_extern_func ~modul (env : Link.Symbolic.State.t) id =
  Link.Symbolic.State.get_extern_func env ~modul id

let get_init_code ~modul env = Link.Symbolic.State.get_init_code ~modul env
