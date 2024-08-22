(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

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
