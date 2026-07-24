(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let get_memory ~(modul : int) link_state id :
  Symbolic_memory.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let* memories = Symbolic_choice.fold_state (fun state -> state.memories) in
  match Thread.Collection.find memories ~module_id:modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    begin match Link.State.get_memory link_state ~modul id with
    | Error _e -> assert false
    | Ok original ->
      let symbolic =
        Symbolic_memory.of_concrete ~module_id:modul ~id original
      in
      let* () = Symbolic_memory.replace symbolic in
      Symbolic_choice.return symbolic
    end

let get_table ~(modul : int) link_state id : Symbolic_table.t Symbolic_choice.t
    =
  let ( let* ) = Symbolic_choice.( let* ) in
  let* tables = Symbolic_choice.fold_state (fun state -> state.tables) in
  match Thread.Collection.find tables ~module_id:modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    begin match Link.State.get_table link_state ~modul id with
    | Error _e -> assert false
    | Ok original ->
      let symbolic = Symbolic_table.of_concrete ~module_id:modul ~id original in
      let* () = Symbolic_table.replace symbolic in
      Symbolic_choice.return symbolic
    end

let get_data ~(modul : int) link_state n =
  match Link.State.get_data ~modul link_state n with
  | Error e -> Symbolic_choice.trap e
  | Ok orig_data -> Symbolic_choice.return orig_data

let get_global ~(modul : int) link_state id :
  Symbolic_global.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let* globals = Symbolic_choice.fold_state (fun state -> state.globals) in
  match Thread.Collection.find globals ~module_id:modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    begin match Link.State.get_global link_state ~modul id with
    | Error _e -> assert false
    | Ok original ->
      let symbolic =
        Symbolic_global.of_concrete ~module_id:modul ~id original
      in
      let* () = Symbolic_global.replace symbolic in
      Symbolic_choice.return symbolic
    end

let get_func ~modul link_state id = Link.State.get_func link_state ~modul id

let get_elem ~modul link_state id = Link.State.get_elem link_state ~modul id

let get_extern_func ~modul (link_state : Symbolic_extern.Func.t Link.State.t) id
    =
  Link.State.get_extern_func link_state ~modul id
