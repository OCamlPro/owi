(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*  Written by Léo Andrès and Pierre Chambart                                *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

open Text
open Syntax

type env =
  { start : bool
  ; memory : bool
  ; imported_memory : bool
  ; funcs : bool
  ; tables : bool
  ; globals : bool
  }

let empty_env () =
  { start = false
  ; memory = false
  ; imported_memory = false
  ; funcs = false
  ; tables = false
  ; globals = false
  }

let modul m =
  Log.debug0 "checking     ...@\n";
  let add_global =
    let seen = Hashtbl.create 512 in
    function
    | None -> Ok ()
    | Some id ->
      if Hashtbl.mem seen id then Error (`Duplicate_global id)
      else Ok (Hashtbl.replace seen id ())
  in
  let add_table =
    let seen = Hashtbl.create 512 in
    function
    | None -> Ok ()
    | Some id ->
      if Hashtbl.mem seen id then Error (`Duplicate_table id)
      else Ok (Hashtbl.replace seen id ())
  in
  let add_memory =
    let seen = Hashtbl.create 512 in
    function
    | None -> Ok ()
    | Some id ->
      if Hashtbl.mem seen id then Error (`Duplicate_memory id)
      else Ok (Hashtbl.add seen id ())
  in

  let+ (_env : env) =
    List.fold_left
      (fun env field ->
        let* env in
        match field with
        | MExport _e -> Ok env
        | MFunc _f -> Ok { env with funcs = true }
        | MStart _start ->
          if env.start then Error `Multiple_start_sections
          else Ok { env with start = true }
        | MImport i ->
          if env.funcs then Error `Import_after_function
          else if env.memory then Error `Import_after_memory
          else if env.tables then Error `Import_after_table
          else if env.globals then Error `Import_after_global
          else begin
            match i.desc with
            | Import_mem (id, _) ->
              let* () = add_memory id in
              if env.imported_memory then Error `Multiple_memories
              else Ok { env with imported_memory = true }
            | Import_func _ -> Ok env
            | Import_global (id, _) ->
              let+ () = add_global id in
              env
            | Import_table (id, _) ->
              let+ () = add_table id in
              env
          end
        | MData _d -> Ok env
        | MElem _e -> Ok env
        | MMem (id, _) ->
          let* () = add_memory id in
          if env.memory || env.imported_memory then Error `Multiple_memories
          else Ok { env with memory = true }
        | MType _t -> Ok env
        | MGlobal { id; _ } ->
          let+ () = add_global id in
          { env with globals = true }
        | MTable (id, _) ->
          let+ () = add_table id in
          { env with tables = true } )
      (Ok (empty_env ()))
      m.fields
  in

  m
