(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Text

let sep fmt () = Fmt.pf fmt " ; "

type type_check = indice * func_type

let pp_type_check fmt (indice, func_type) =
  Fmt.pf fmt "(%a, %a)" pp_indice indice pp_func_type func_type

type opt_export =
  { name : string
  ; id : indice
  }

let pp_opt_export fmt { name; id } = Fmt.pf fmt "(%S, %a)" name pp_indice id

let pp_opt_export_list fmt l = Fmt.pf fmt "[%a]" (Fmt.list ~sep pp_opt_export) l

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

let pp_opt_exports fmt { global; mem; table; func } =
  Fmt.pf fmt "{@\n  @[<v>global: %a@\nmem: %a@\ntable: %a@\nfunc: %a@\n@]}"
    pp_opt_export_list global pp_opt_export_list mem pp_opt_export_list table
    pp_opt_export_list func

let curr_id (curr : int ref) (i : indice option) =
  match i with None -> Raw (pred !curr) | Some id -> id

type t =
  { id : string option
  ; typ : Typedef.t list
  ; function_type : func_type list
      (* Types comming from function declarations.
     It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
     Come from function declarations with type indicies *)
  ; global : (Text.Global.t, Global.Type.t) Runtime.t Indexed.t list
  ; table : (Table.t, Table.Type.t) Runtime.t Indexed.t list
  ; mem : (mem, limits) Runtime.t Indexed.t list
  ; func : (Func.t, block_type) Runtime.t Indexed.t list
  ; elem : Text.Elem.t Indexed.t list
  ; data : Text.Data.t Indexed.t list
  ; exports : opt_exports
  ; start : indice option
  }

let pp_id fmt id = Text.pp_id_opt fmt id

let pp_typ fmt typ = Fmt.pf fmt "[%a]" (Fmt.list ~sep Text.Typedef.pp) typ

let pp_function_type fmt function_type =
  Fmt.pf fmt "[%a]" (Fmt.list ~sep Text.pp_func_type) function_type

let pp_type_checks fmt type_checks =
  Fmt.pf fmt "[%a]" (Fmt.list ~sep pp_type_check) type_checks

let pp_runtime_indexed_list ~pp_local ~pp_imported fmt l =
  Indexed.pp_list (Runtime.pp ~pp_local ~pp_imported) fmt l

let pp_global fmt g =
  pp_runtime_indexed_list ~pp_local:Text.Global.pp
    ~pp_imported:Text.Global.Type.pp fmt g

let pp_table fmt t =
  pp_runtime_indexed_list ~pp_local:Text.Table.pp
    ~pp_imported:Text.Table.Type.pp fmt t

let pp_mem fmt m =
  pp_runtime_indexed_list ~pp_local:Text.pp_mem ~pp_imported:Text.pp_limits fmt
    m

let pp_func fmt f =
  pp_runtime_indexed_list ~pp_local:Text.Func.pp ~pp_imported:Text.pp_block_type
    fmt f

let pp_elem fmt e = Indexed.pp_list Text.Elem.pp fmt e

let pp_data fmt d = Indexed.pp_list Text.Data.pp fmt d

let pp_start fmt s = Text.pp_indice_opt fmt s

let pp fmt
  { id
  ; typ
  ; function_type
  ; type_checks
  ; global
  ; table
  ; mem
  ; func
  ; elem
  ; data
  ; exports
  ; start
  } =
  Fmt.pf fmt
    "{id: %a@\n\
    \  @[<v>typ: %a@\n\
     function_type: %a@\n\
     type_checks: %a@\n\
     global: %a@\n\
     table: %a@\n\
     mem: %a@\n\
     func: %a@\n\
     elem: %a@\n\
     data: %a@\n\
     exports: %a@\n\
     start: %a@\n\
     }"
    pp_id id pp_typ typ pp_function_type function_type pp_type_checks
    type_checks pp_global global pp_table table pp_mem mem pp_func func pp_elem
    elem pp_data data pp_opt_exports exports pp_start start

let imp (import : Import.t) (assigned_name, desc) : 'a Imported.t =
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
    match instr.Annotated.raw with
    | Block (_str_opt, bt, expr1) | Loop (_str_opt, bt, expr1) ->
      Option.to_list bt @ extract_block_types expr1
    | If_else (_str_opt, bt, expr1, expr2) ->
      Option.to_list bt @ extract_block_types expr1 @ extract_block_types expr2
    | Return_call_indirect (_, bt) | Return_call_ref bt | Call_indirect (_, bt)
      ->
      [ bt ]
    | _ -> []
  in
  List.concat_map aux expr.raw

let add_func value (fields : t) (curr : curr) =
  let fields =
    match value with
    | Runtime.Local (func : Func.t) ->
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
  | Text.Module.Field.Typedef typ ->
    let typ = typ :: fields.typ in
    { fields with typ }
  | Global global -> add_global (Local global) fields curr
  | Import ({ typ = Global (a, (mut, val_type)); _ } as import) ->
    let b = (mut, val_type) in
    let imported = imp import (a, b) in
    add_global (Imported imported) fields curr
  | Export { name; typ = Global id } ->
    let id = curr_id curr.global id in
    let exports =
      { fields.exports with global = { name; id } :: fields.exports.global }
    in
    { fields with exports }
  | Table table ->
    let id, table_type = table in
    let table = (id, table_type) in
    add_table (Local table) fields curr
  | Import ({ typ = Table (id, table_type); _ } as import) ->
    let imported = imp import (id, table_type) in
    add_table (Imported imported) fields curr
  | Export { name; typ = Table id } ->
    let id = curr_id curr.table id in
    let exports =
      { fields.exports with table = { name; id } :: fields.exports.table }
    in
    { fields with exports }
  | Mem mem -> add_mem (Local mem) fields curr
  | Import ({ typ = Mem (id, limits); _ } as import) ->
    let imported = imp import (id, limits) in
    add_mem (Imported imported) fields curr
  | Export { name; typ = Mem id } ->
    let id = curr_id curr.mem id in
    let exports =
      { fields.exports with mem = { name; id } :: fields.exports.mem }
    in
    { fields with exports }
  | Func func -> add_func (Runtime.Local func) fields curr
  | Import ({ typ = Func (a, type_f); _ } as import) ->
    let imported : block_type Imported.t = imp import (a, type_f) in
    add_func (Imported imported) fields curr
  | Export { name; typ = Func id } ->
    let id = curr_id curr.func id in
    let exports =
      { fields.exports with func = { name; id } :: fields.exports.func }
    in
    { fields with exports }
  | Elem elem ->
    let mode =
      match elem.mode with
      | (Text.Elem.Mode.Passive | Declarative) as mode -> mode
      | Active (id, expr) ->
        let id = curr_id curr.table id in
        Active (Some id, expr)
    in
    add_elem { elem with mode } fields curr
  | Data data ->
    let mode =
      match data.mode with
      | Passive -> Text.Data.Mode.Passive
      | Active (id, expr) ->
        let id = curr_id curr.mem id in
        Active (Some id, expr)
    in
    let data : Text.Data.t = { id = data.id; init = data.init; mode } in
    add_data data fields curr
  | Start start -> { fields with start = Some start }

let of_text { Text.Module.fields; id } =
  Log.debug (fun m -> m "grouping     ...");
  let modul =
    List.fold_left (add_field (init_curr ())) (empty_module id) fields
  in
  Log.debug (fun m -> m "%a" pp modul);
  modul
