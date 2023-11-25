(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types
open Syntax

type type_check = text indice * text func_type

type opt_ind =
  | Curr of int
  | Indice of text indice

type opt_export =
  { name : string
  ; id : opt_ind
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

let curr_id (curr : int) (i : text indice option) =
  match i with None -> Curr (pred curr) | Some id -> Indice id

type t =
  { id : string option
  ; typ : text type_def list
  ; function_type : text func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Text.global, simplified global_type) Runtime.t Indexed.t list
  ; table : (simplified table, simplified table_type) Runtime.t Indexed.t list
  ; mem : (mem, limits) Runtime.t Indexed.t list
  ; func : (text func, text block_type) Runtime.t Indexed.t list
  ; elem : Text.elem Indexed.t list
  ; data : Text.data Indexed.t list
  ; exports : opt_exports
  ; start : text indice option
  }

let imp (import : text import) (assigned_name, desc) : 'a Imported.t =
  { modul = import.modul; name = import.name; assigned_name; desc }

let empty_module id =
  { id
  ; typ = []
  ; function_type = []
  ; type_checks = []
  ; global = []
  ; table = []
  ; mem = []
  ; func = []
  ; elem = []
  ; data = []
  ; exports = { global = []; table = []; mem = []; func = [] }
  ; start = None
  }

type curr =
  { global : int
  ; table : int
  ; mem : int
  ; func : int
  ; elem : int
  ; data : int
  }

let init_curr = { global = 0; table = 0; mem = 0; func = 0; elem = 0; data = 0 }

let add_global value (fields : t) (curr : curr) =
  let index = curr.global in
  ( { fields with global = Indexed.return index value :: fields.global }
  , { curr with global = succ curr.global } )

let add_table value (fields : t) (curr : curr) =
  let index = curr.table in
  ( { fields with table = Indexed.return index value :: fields.table }
  , { curr with table = succ curr.table } )

let add_mem value (fields : t) (curr : curr) =
  let index = curr.mem in
  ( { fields with mem = Indexed.return index value :: fields.mem }
  , { curr with mem = succ curr.mem } )

let add_func value (fields : t) (curr : curr) =
  let index = curr.func in
  ( { fields with func = Indexed.return index value :: fields.func }
  , { curr with func = succ curr.func } )

let add_elem value (fields : t) (curr : curr) =
  let index = curr.elem in
  ( { fields with elem = Indexed.return index value :: fields.elem }
  , { curr with elem = succ curr.elem } )

let add_data value (fields : t) (curr : curr) =
  let index = curr.data in
  ( { fields with data = Indexed.return index value :: fields.data }
  , { curr with data = succ curr.data } )

let check_limit { min; max } =
  match max with
  | None -> Ok ()
  | Some max ->
    if min > max then Error "size minimum must not be greater than maximum"
    else Ok ()

let of_symbolic (modul : Text.modul) : t Result.t =
  Log.debug0 "grouping     ...@\n";
  let add ((fields : t), curr) field : (t * curr) Result.t =
    match field with
    | Text.MType typ ->
      let typ = typ @ fields.typ in
      ok @@ ({ fields with typ }, curr)
    | MGlobal global -> ok @@ add_global (Local global) fields curr
    | MImport ({ desc = Import_global (a, (mut, val_type)); _ } as import) ->
      let+ val_type = Simplified_types.convert_val_type None val_type in
      let b = (mut, val_type) in
      let imported = imp import (a, b) in
      add_global (Imported imported) fields curr
    | MExport { name; desc = Export_global id } ->
      let id = curr_id curr.global id in
      let exports =
        { fields.exports with global = { name; id } :: fields.exports.global }
      in
      ok ({ fields with exports }, curr)
    | MTable table ->
      let _, (limits, _) = table in
      let* () = check_limit limits in
      let id, table_type = table in
      let+ table_type = Simplified_types.convert_table_type None table_type in
      let table = (id, table_type) in
      add_table (Local table) fields curr
    | MImport ({ desc = Import_table (id, table_type); _ } as import) ->
      let+ table_type = Simplified_types.convert_table_type None table_type in
      let imported = imp import (id, table_type) in
      add_table (Imported imported) fields curr
    | MExport { name; desc = Export_table id } ->
      let id = curr_id curr.table id in
      let exports =
        { fields.exports with table = { name; id } :: fields.exports.table }
      in
      ok ({ fields with exports }, curr)
    | MMem ((_, limits) as mem) ->
      let* () =
        if limits.min > 65536 then
          Error "memory size must be at most 65536 pages (4GiB)"
        else Ok ()
      in
      let* () =
        match limits.max with
        | Some max when max > 65536 ->
          Error "memory size must be at most 65536 pages (4GiB)"
        | Some _ | None -> Ok ()
      in
      let* () = check_limit limits in
      ok @@ add_mem (Local mem) fields curr
    | MImport ({ desc = Import_mem (id, limits); _ } as import) ->
      let imported = imp import (id, limits) in
      ok @@ add_mem (Imported imported) fields curr
    | MExport { name; desc = Export_mem id } ->
      let id = curr_id curr.mem id in
      let exports =
        { fields.exports with mem = { name; id } :: fields.exports.mem }
      in
      Ok ({ fields with exports }, curr)
    | MFunc func ->
      let function_type, type_checks =
        match func.type_f with
        | Bt_ind _ -> (fields.function_type, fields.type_checks)
        | Bt_raw (id, typ) ->
          let type_checks =
            match id with
            | None -> fields.type_checks
            | Some id -> (id, typ) :: fields.type_checks
          in
          (typ :: fields.function_type, type_checks)
      in
      let index = curr.func in
      let value = Runtime.Local func in
      let func = Indexed.return index value :: fields.func in
      Ok
        ( { fields with func; function_type; type_checks }
        , { curr with func = succ curr.func } )
    | MImport ({ desc = Import_func (a, b); _ } as import) ->
      let imported : text block_type Imported.t = imp import (a, b) in
      ok @@ add_func (Imported imported) fields curr
    | MExport { name; desc = Export_func id } ->
      let id = curr_id curr.func id in
      let exports =
        { fields.exports with func = { name; id } :: fields.exports.func }
      in
      Ok ({ fields with exports }, curr)
    | MElem elem ->
      let mode =
        match elem.mode with
        | (Text.Elem_passive | Elem_declarative) as mode -> mode
        | Elem_active (id, expr) ->
          let id = Option.value id ~default:(Raw (curr.table - 1)) in
          Elem_active (Some id, expr)
      in
      ok @@ add_elem { elem with mode } fields curr
    | MData data ->
      let mode =
        match data.mode with
        | Data_passive -> Text.Data_passive
        | Data_active (id, expr) ->
          let id = Option.value id ~default:(Raw (curr.mem - 1)) in
          Data_active (Some id, expr)
      in
      let data : Text.data = { id = data.id; init = data.init; mode } in
      ok @@ add_data data fields curr
    | MStart start -> Ok ({ fields with start = Some start }, curr)
  in
  let+ modul, _curr =
    list_fold_left add (empty_module modul.id, init_curr) modul.fields
  in
  modul
