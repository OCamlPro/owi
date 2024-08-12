(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax

type type_check = text indice * text func_type

type opt_export =
  { name : string
  ; id : text indice
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

let curr_id (curr : int ref) (i : text indice option) =
  match i with None -> Raw (pred !curr) | Some id -> id

type t =
  { id : string option
  ; typ : text type_def list
  ; function_type : text func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Text.global, binary global_type) Runtime.t Indexed.t list
  ; table : (binary table, binary table_type) Runtime.t Indexed.t list
  ; mem : (mem, limits) Runtime.t Indexed.t list
  ; func : (text func, text block_type) Runtime.t Indexed.t list
  ; elem : Text.elem Indexed.t list
  ; data : Text.data Indexed.t list
  ; exports : opt_exports
  ; start : text indice option
  ; annots : text Annot.annot list
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
  ; annots = []
  }

type curr =
  { global : int ref
  ; table : int ref
  ; mem : int ref
  ; func : int ref
  ; elem : int ref
  ; data : int ref
  }

let init_curr () =
  { global = ref 0
  ; table = ref 0
  ; mem = ref 0
  ; func = ref 0
  ; elem = ref 0
  ; data = ref 0
  }

let declare_func_type (fields : t) type_f =
  match type_f with
  | Bt_ind _ -> fields
  | Bt_raw (id, typ) ->
    let type_checks =
      match id with
      | None -> fields.type_checks
      | Some id -> (id, typ) :: fields.type_checks
    in
    { fields with function_type = typ :: fields.function_type; type_checks }

let add_global value (fields : t) (curr : curr) =
  let index = !(curr.global) in
  incr curr.global;
  { fields with global = Indexed.return index value :: fields.global }

let add_table value (fields : t) (curr : curr) =
  let index = !(curr.table) in
  incr curr.table;
  { fields with table = Indexed.return index value :: fields.table }

let add_mem value (fields : t) (curr : curr) =
  let index = !(curr.mem) in
  incr curr.mem;
  { fields with mem = Indexed.return index value :: fields.mem }

let rec extract_block_types expr =
  let aux instr =
    match instr with
    | Block (_str_opt, bt, expr1) | Loop (_str_opt, bt, expr1) ->
      Option.to_list bt @ extract_block_types expr1
    | If_else (_str_opt, bt, expr1, expr2) ->
      Option.to_list bt @ extract_block_types expr1 @ extract_block_types expr2
    | Return_call_indirect (_, bt) | Return_call_ref bt | Call_indirect (_, bt)
      ->
      [ bt ]
    | _ -> []
  in
  List.concat_map aux expr

let add_func value (fields : t) (curr : curr) =
  let fields =
    match value with
    | Runtime.Local func ->
      let fields = declare_func_type fields func.type_f in
      List.fold_left declare_func_type fields (extract_block_types func.body)
    | Imported func -> declare_func_type fields func.desc
  in
  let index = !(curr.func) in
  incr curr.func;
  { fields with func = Indexed.return index value :: fields.func }

let add_elem value (fields : t) (curr : curr) =
  let index = !(curr.elem) in
  incr curr.elem;
  { fields with elem = Indexed.return index value :: fields.elem }

let add_data value (fields : t) (curr : curr) =
  let index = !(curr.data) in
  incr curr.data;
  { fields with data = Indexed.return index value :: fields.data }

let add_field curr (fields : t) = function
  | Text.MType typ ->
    let typ = typ @ fields.typ in
    Ok { fields with typ }
  | MGlobal global -> ok @@ add_global (Local global) fields curr
  | MImport ({ desc = Import_global (a, (mut, val_type)); _ } as import) ->
    let+ val_type = Binary_types.convert_val_type None val_type in
    let b = (mut, val_type) in
    let imported = imp import (a, b) in
    add_global (Imported imported) fields curr
  | MExport { name; desc = Export_global id } ->
    let id = curr_id curr.global id in
    let exports =
      { fields.exports with global = { name; id } :: fields.exports.global }
    in
    Ok { fields with exports }
  | MTable table ->
    let id, table_type = table in
    let+ table_type = Binary_types.convert_table_type None table_type in
    let table = (id, table_type) in
    add_table (Local table) fields curr
  | MImport ({ desc = Import_table (id, table_type); _ } as import) ->
    let+ table_type = Binary_types.convert_table_type None table_type in
    let imported = imp import (id, table_type) in
    add_table (Imported imported) fields curr
  | MExport { name; desc = Export_table id } ->
    let id = curr_id curr.table id in
    let exports =
      { fields.exports with table = { name; id } :: fields.exports.table }
    in
    Ok { fields with exports }
  | MMem mem -> ok @@ add_mem (Local mem) fields curr
  | MImport ({ desc = Import_mem (id, limits); _ } as import) ->
    let imported = imp import (id, limits) in
    ok @@ add_mem (Imported imported) fields curr
  | MExport { name; desc = Export_mem id } ->
    let id = curr_id curr.mem id in
    let exports =
      { fields.exports with mem = { name; id } :: fields.exports.mem }
    in
    Ok { fields with exports }
  | MFunc func -> ok @@ add_func (Runtime.Local func) fields curr
  | MImport ({ desc = Import_func (a, type_f); _ } as import) ->
    let imported : text block_type Imported.t = imp import (a, type_f) in
    ok @@ add_func (Imported imported) fields curr
  | MExport { name; desc = Export_func id } ->
    let id = curr_id curr.func id in
    let exports =
      { fields.exports with func = { name; id } :: fields.exports.func }
    in
    Ok { fields with exports }
  | MElem elem ->
    let mode =
      match elem.mode with
      | (Text.Elem_passive | Elem_declarative) as mode -> mode
      | Elem_active (id, expr) ->
        let id = curr_id curr.table id in
        Elem_active (Some id, expr)
    in
    ok @@ add_elem { elem with mode } fields curr
  | MData data ->
    let mode =
      match data.mode with
      | Data_passive -> Text.Data_passive
      | Data_active (id, expr) ->
        let id = curr_id curr.mem id in
        Data_active (Some id, expr)
    in
    let data : Text.data = { id = data.id; init = data.init; mode } in
    ok @@ add_data data fields curr
  | MStart start -> Ok { fields with start = Some start }

let of_symbolic { Text.fields; id; annots } =
  Log.debug0 "grouping     ...@\n";
  let+ modul = list_fold_left (add_field (init_curr ())) (empty_module id) fields
  in
  { modul with annots }
