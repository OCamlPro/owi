(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Text
open Syntax

type env =
  { start : bool
  ; declared_memory : bool
  ; funcs : bool
  ; tables : bool
  ; globals : bool
  }

let empty_env () =
  { start = false
  ; declared_memory = false
  ; funcs = false
  ; tables = false
  ; globals = false
  }

let modul m =
  Log.info (fun m -> m "checking     ...");
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
    let open Module in
    list_fold_left
      (fun env ->
        let open Field in
        function
        | Export _e -> Ok env
        | Func _f -> Ok { env with funcs = true }
        | Start _start ->
          if env.start then Error `Multiple_start_sections
          else Ok { env with start = true }
        | Import i ->
          if env.funcs then Error `Import_after_function
          else if env.declared_memory then Error `Import_after_memory
          else if env.tables then Error `Import_after_table
          else if env.globals then Error `Import_after_global
          else begin
            match i.typ with
            | Mem (id, _) ->
              let* () = add_memory id in
              Ok env
            | Func _ -> Ok env
            | Global (id, _) ->
              let+ () = add_global id in
              env
            | Table (id, _) ->
              let+ () = add_table id in
              env
          end
        | Data _d -> Ok env
        | Elem _e -> Ok env
        | Mem (id, _) ->
          let* () = add_memory id in
          Ok { env with declared_memory = true }
        | Typedef _t -> Ok env
        | Global { id; _ } ->
          let+ () = add_global id in
          { env with globals = true }
        | Table (id, _) ->
          let+ () = add_table id in
          { env with tables = true } )
      (empty_env ()) m.fields
  in

  m
