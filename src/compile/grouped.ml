(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Text

type opt_export =
  { name : string
  ; id : indice
  }

let curr_id (curr : int) (i : indice option) =
  match i with None -> Raw (pred curr) | Some id -> id

type t =
  { id : string option
  ; typ : Typedef.t Dynarray.t
  ; function_type : func_type Dynarray.t
      (** Types comming from function declarations. It contains potential
          duplication. *)
  ; type_checks : (indice * func_type) Dynarray.t
      (** Types checks to perform after assignment. Come from function
          declarations with type indicies. *)
  ; global : (Text.Global.t, Global.Type.t) Origin.t Dynarray.t
  ; table : (Table.t, Table.Type.t) Origin.t Dynarray.t
  ; mem : (Mem.t, limits) Origin.t Dynarray.t
  ; func : (Func.t, block_type) Origin.t Dynarray.t
  ; elem : Text.Elem.t Dynarray.t
  ; data : Text.Data.t Dynarray.t
  ; global_exports : opt_export Dynarray.t
  ; mem_exports : opt_export Dynarray.t
  ; table_exports : opt_export Dynarray.t
  ; func_exports : opt_export Dynarray.t
  ; mutable start : indice option
  }

let pp_id fmt id = Text.pp_id_opt fmt id

let pp_dynarray pp_v fmt values =
  Fmt.pf fmt "[%a]"
    (Fmt.iter ~sep:(fun fmt () -> Fmt.pf fmt " ; ") Dynarray.iter pp_v)
    values

let pp_typ fmt typ = pp_dynarray Text.Typedef.pp fmt typ

let pp_function_type fmt function_type =
  pp_dynarray Text.pp_func_type fmt function_type

let pp_type_check fmt (indice, func_type) =
  Fmt.pf fmt "(%a, %a)" pp_indice indice pp_func_type func_type

let pp_type_checks fmt type_checks = pp_dynarray pp_type_check fmt type_checks

let pp_runtime_dynarray ~pp_local ~pp_imported fmt l =
  pp_dynarray (Origin.pp ~pp_local ~pp_imported) fmt l

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
  ; start
  ; _
  } =
  (* TODO: print exports once again *)
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
     start: %a@\n\
     }"
    pp_id id pp_typ typ pp_function_type function_type pp_type_checks
    type_checks pp_global global pp_table table pp_mem mem pp_func func pp_elem
    elem pp_data data pp_start start

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
  ; global_exports = Dynarray.create ()
  ; mem_exports = Dynarray.create ()
  ; table_exports = Dynarray.create ()
  ; func_exports = Dynarray.create ()
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
  | Origin.Imported func -> add_func_type modul func.typ
  | Local (func : Func.t) ->
    List.iter (add_func_type modul)
      (func.type_f :: extract_block_types func.body)
  end;
  Dynarray.add_last modul.func value

let add_elem value (modul : t) = Dynarray.add_last modul.elem value

let add_data value (modul : t) = Dynarray.add_last modul.data value

let add_typ value (modul : t) = Dynarray.add_last modul.typ value

let add_field (modul : t) : Text.Module.Field.t -> unit = function
  | Typedef typ -> add_typ typ modul
  | Global global -> add_global (Local global) modul
  | Import { typ = Global (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    add_global imported modul
  | Export { name; typ = Global id } ->
    let id = curr_id (Dynarray.length modul.global) id in
    Dynarray.add_last modul.global_exports { name; id }
  | Table table ->
    let id, table_type = table in
    let table = (id, table_type) in
    add_table (Local table) modul
  | Import { typ = Table (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    add_table imported modul
  | Export { name; typ = Table id } ->
    let id = curr_id (Dynarray.length modul.table) id in
    Dynarray.add_last modul.table_exports { name; id }
  | Mem mem -> add_mem (Local mem) modul
  | Import { typ = Mem (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    add_mem imported modul
  | Export { name; typ = Mem id } ->
    let id = curr_id (Dynarray.length modul.mem) id in
    Dynarray.add_last modul.mem_exports { name; id }
  | Func func -> add_func (Origin.Local func) modul
  | Import { typ = Func (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    add_func imported modul
  | Export { name; typ = Func id } ->
    let id = curr_id (Dynarray.length modul.func) id in
    Dynarray.add_last modul.func_exports { name; id }
  | Elem elem ->
    let mode =
      match elem.mode with
      | (Text.Elem.Mode.Passive | Declarative) as mode -> mode
      | Active (id, expr) ->
        let id = curr_id (Dynarray.length modul.table) id in
        Active (Some id, expr)
    in
    add_elem { elem with mode } modul
  | Data data ->
    let mode =
      match data.mode with
      | Passive -> Text.Data.Mode.Passive
      | Active (id, expr) ->
        let id = curr_id (Dynarray.length modul.mem) id in
        Active (Some id, expr)
    in
    add_data { data with mode } modul
  | Start start -> modul.start <- Some start

let of_text { Text.Module.fields; id } =
  Log.debug (fun m -> m "grouping     ...");
  let modul = empty_module id in
  List.iter (add_field modul) fields;
  Log.debug (fun m -> m "%a" pp modul);
  modul
