(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Link.Linked_module

type nonrec t = Symbolic_extern.Func.t t

let get_memory (modul : t) id : Symbolic_memory.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let module_id = get_id modul in
  let* memories = Symbolic_choice.fold_state (fun state -> state.memories) in
  match Thread.Collection.find memories ~module_id ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    begin match get_memory modul id with
    | Error _e -> assert false
    | Ok original ->
      let symbolic = Symbolic_memory.of_concrete ~module_id ~id original in
      let* () = Symbolic_memory.replace symbolic in
      Symbolic_choice.return symbolic
    end

let get_table (modul : t) id : Symbolic_table.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let module_id = get_id modul in
  let* tables = Symbolic_choice.fold_state (fun state -> state.tables) in
  match Thread.Collection.find tables ~module_id ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    begin match get_table modul id with
    | Error _e -> assert false
    | Ok original ->
      let symbolic = Symbolic_table.of_concrete ~module_id ~id original in
      let* () = Symbolic_table.replace symbolic in
      Symbolic_choice.return symbolic
    end

let get_data modul n =
  match get_data modul n with
  | Error e -> Symbolic_choice.trap e
  | Ok orig_data -> Symbolic_choice.return orig_data

let get_global (modul : t) id : Symbolic_global.t Symbolic_choice.t =
  let ( let* ) = Symbolic_choice.( let* ) in
  let module_id = get_id modul in
  let* globals = Symbolic_choice.fold_state (fun state -> state.globals) in
  match Thread.Collection.find globals ~module_id ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    begin match get_global modul id with
    | Error _e -> assert false
    | Ok original ->
      let symbolic = Symbolic_global.of_concrete ~module_id ~id original in
      let* () = Symbolic_global.replace symbolic in
      Symbolic_choice.return symbolic
    end
