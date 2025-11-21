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

let curr_id (curr : int) (i : indice option) =
  match i with None -> Raw (pred curr) | Some id -> id

type t =
  { id : string option
  ; typ : Typedef.t Dynarray.t
  ; function_type : func_type Dynarray.t
      (** Types comming from function declarations. It contains potential
          duplication. *)
  ; type_checks : type_check Dynarray.t
      (** Types checks to perform after assignment. Come from function
          declarations with type indicies. *)
  ; global : (Text.Global.t, Global.Type.t) Runtime.t Dynarray.t
  ; table : (Table.t, Table.Type.t) Runtime.t Dynarray.t
  ; mem : (Mem.t, limits) Runtime.t Dynarray.t
  ; func : (Func.t, block_type) Runtime.t Dynarray.t
  ; elem : Text.Elem.t Dynarray.t
  ; data : Text.Data.t Dynarray.t
  ; exports : opt_exports
  ; start : indice option
  }

let pp_id fmt id = Text.pp_id_opt fmt id

let pp_dynarray pp_v fmt values =
  Fmt.pf fmt "[%a]"
    (Fmt.iter ~sep:(fun fmt () -> Fmt.pf fmt " ; ") Dynarray.iter pp_v)
    values

let pp_typ fmt typ = pp_dynarray Text.Typedef.pp fmt typ

let pp_function_type fmt function_type =
  pp_dynarray Text.pp_func_type fmt function_type

let pp_type_checks fmt type_checks = pp_dynarray pp_type_check fmt type_checks

let pp_runtime_dynarray ~pp_local ~pp_imported fmt l =
  pp_dynarray (Runtime.pp ~pp_local ~pp_imported) fmt l

let pp_global fmt g =
  pp_runtime_dynarray ~pp_local:Text.Global.pp ~pp_imported:Text.Global.Type.pp
    fmt g

let pp_table fmt t =
  pp_runtime_dynarray ~pp_local:Text.Table.pp ~pp_imported:Text.Table.Type.pp
    fmt t

let pp_mem fmt m =
  pp_runtime_dynarray ~pp_local:Text.Mem.pp ~pp_imported:Text.pp_limits fmt m

let pp_func fmt f =
  pp_runtime_dynarray ~pp_local:Text.Func.pp ~pp_imported:Text.pp_block_type fmt
    f

let pp_elem fmt e = pp_dynarray Text.Elem.pp fmt e

let pp_data fmt d = pp_dynarray Text.Data.pp fmt d

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

let imp (import : Import.t) (assigned_name, typ) : 'a Imported.t =
  { modul = import.modul; name = import.name; assigned_name; typ }

let empty_module id =
  { id
  ; typ = Dynarray.create ()
  ; function_type = Dynarray.create ()
  ; type_checks = Dynarray.create ()
  ; global = Dynarray.create ()
  ; table = Dynarray.create ()
  ; mem = Dynarray.create ()
  ; func = Dynarray.create ()
  ; elem = Dynarray.create ()
  ; data = Dynarray.create ()
  ; exports = { global = []; table = []; mem = []; func = [] }
  ; start = None
  }

let add_func_type modul = function
  | Bt_ind _ -> ()
  | Bt_raw (id, typ) ->
    Dynarray.add_last modul.function_type typ;
    Option.iter (fun id -> Dynarray.add_last modul.type_checks (id, typ)) id

let add_global value modul = Dynarray.add_last modul.global value

let add_table value modul = Dynarray.add_last modul.table value

let add_mem value modul = Dynarray.add_last modul.mem value

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

let add_func value (modul : t) =
  begin match value with
  | Runtime.Imported func -> add_func_type modul func.typ
  | Local (func : Func.t) ->
    List.iter (add_func_type modul)
      (func.type_f :: extract_block_types func.body)
  end;
  Dynarray.add_last modul.func value

let add_elem value (modul : t) = Dynarray.add_last modul.elem value

let add_data value (modul : t) = Dynarray.add_last modul.data value

let add_typ value (modul : t) = Dynarray.add_last modul.typ value

let add_field (modul : t) : Text.Module.Field.t -> t = function
  | Typedef typ ->
    add_typ typ modul;
    modul
  | Global global ->
    add_global (Local global) modul;
    modul
  | Import ({ typ = Global (a, (mut, val_type)); _ } as import) ->
    let b = (mut, val_type) in
    let imported = imp import (a, b) in
    add_global (Imported imported) modul;
    modul
  | Export { name; typ = Global id } ->
    let exports =
      let id = curr_id (Dynarray.length modul.global) id in
      { modul.exports with global = { name; id } :: modul.exports.global }
    in
    { modul with exports }
  | Table table ->
    let id, table_type = table in
    let table = (id, table_type) in
    add_table (Local table) modul;
    modul
  | Import ({ typ = Table (id, table_type); _ } as import) ->
    let imported = imp import (id, table_type) in
    add_table (Imported imported) modul;
    modul
  | Export { name; typ = Table id } ->
    let exports =
      let id = curr_id (Dynarray.length modul.table) id in
      { modul.exports with table = { name; id } :: modul.exports.table }
    in
    { modul with exports }
  | Mem mem ->
    add_mem (Local mem) modul;
    modul
  | Import ({ typ = Mem (id, limits); _ } as import) ->
    let imported = imp import (id, limits) in
    add_mem (Imported imported) modul;
    modul
  | Export { name; typ = Mem id } ->
    let exports =
      let id = curr_id (Dynarray.length modul.mem) id in
      { modul.exports with mem = { name; id } :: modul.exports.mem }
    in
    { modul with exports }
  | Func func ->
    add_func (Runtime.Local func) modul;
    modul
  | Import ({ typ = Func (a, type_f); _ } as import) ->
    let imported : block_type Imported.t = imp import (a, type_f) in
    add_func (Imported imported) modul;
    modul
  | Export { name; typ = Func id } ->
    let exports =
      let id = curr_id (Dynarray.length modul.func) id in
      { modul.exports with func = { name; id } :: modul.exports.func }
    in
    { modul with exports }
  | Elem elem ->
    let mode =
      match elem.mode with
      | (Text.Elem.Mode.Passive | Declarative) as mode -> mode
      | Active (id, expr) ->
        let id = curr_id (Dynarray.length modul.table) id in
        Active (Some id, expr)
    in
    add_elem { elem with mode } modul;
    modul
  | Data data ->
    let mode =
      match data.mode with
      | Passive -> Text.Data.Mode.Passive
      | Active (id, expr) ->
        let id = curr_id (Dynarray.length modul.mem) id in
        Active (Some id, expr)
    in
    let data : Text.Data.t = { id = data.id; init = data.init; mode } in
    add_data data modul;
    modul
  | Start start -> { modul with start = Some start }

let of_text { Text.Module.fields; id } =
  Log.debug (fun m -> m "grouping     ...");
  let modul = empty_module id in
  let modul = List.fold_left add_field modul fields in
  Log.debug (fun m -> m "%a" pp modul);
  modul
