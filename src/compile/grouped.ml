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
  ; typ : Typedef.t Array.t
  ; function_type : func_type Array.t
      (** Types comming from function declarations. It contains potential
          duplication. *)
  ; type_checks : (indice * func_type) Array.t
      (** Types checks to perform after assignment. Come from function
          declarations with type indicies. *)
  ; global : (Text.Global.t, Global.Type.t) Origin.t Array.t
  ; table : (Table.t, Table.Type.t) Origin.t Array.t
  ; mem : (Mem.t, limits) Origin.t Array.t
  ; func : (Func.t, block_type) Origin.t Array.t
  ; elem : Text.Elem.t Array.t
  ; data : Text.Data.t Array.t
  ; global_exports : opt_export Array.t
  ; mem_exports : opt_export Array.t
  ; table_exports : opt_export Array.t
  ; func_exports : opt_export Array.t
  ; start : indice option
  }

let pp_id fmt id = Text.pp_id_opt fmt id

let pp_typ fmt typ = Fmt.array Text.Typedef.pp fmt typ

let pp_function_type fmt function_type =
  Fmt.array Text.pp_func_type fmt function_type

let pp_type_check fmt (indice, func_type) =
  Fmt.pf fmt "(%a, %a)" pp_indice indice pp_func_type func_type

let pp_type_checks fmt type_checks = Fmt.array pp_type_check fmt type_checks

let pp_runtime_array ~pp_local ~pp_imported fmt l =
  Fmt.array (Origin.pp ~pp_local ~pp_imported) fmt l

let pp_global fmt g =
  pp_runtime_array ~pp_local:Text.Global.pp ~pp_imported:Text.Global.Type.pp fmt
    g

let pp_table fmt t =
  pp_runtime_array ~pp_local:Text.Table.pp ~pp_imported:Text.Table.Type.pp fmt t

let pp_mem fmt m =
  pp_runtime_array ~pp_local:Text.Mem.pp ~pp_imported:Text.pp_limits fmt m

let pp_func fmt f =
  pp_runtime_array ~pp_local:Text.Func.pp ~pp_imported:Text.pp_block_type fmt f

let pp_elem fmt e = Fmt.array Text.Elem.pp fmt e

let pp_data fmt d = Fmt.array Text.Data.pp fmt d

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

let add_func_type function_type type_checks = function
  | Bt_ind _ -> ()
  | Bt_raw (id, typ) ->
    Dynarray.add_last function_type typ;
    Option.iter (fun id -> Dynarray.add_last type_checks (id, typ)) id

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

let add_func value function_type type_checks func =
  begin match value with
  | Origin.Imported f -> add_func_type function_type type_checks f.typ
  | Local (f : Func.t) ->
    List.iter
      (add_func_type function_type type_checks)
      (f.type_f :: extract_block_types f.body)
  end;
  Dynarray.add_last func value

let add_field typ function_type type_checks global table mem func elem data
  global_exports mem_exports table_exports func_exports start :
  Text.Module.Field.t -> unit = function
  | Typedef t -> Dynarray.add_last typ t
  | Global g -> Dynarray.add_last global (Origin.Local g)
  | Import { typ = Global (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    Dynarray.add_last global imported
  | Export { name; typ = Global id } ->
    let id = curr_id (Dynarray.length global) id in
    Dynarray.add_last global_exports { name; id }
  | Table tbl ->
    let id, table_type = tbl in
    let tbl = (id, table_type) in
    Dynarray.add_last table (Origin.Local tbl)
  | Import { typ = Table (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    Dynarray.add_last table imported
  | Export { name; typ = Table id } ->
    let id = curr_id (Dynarray.length table) id in
    Dynarray.add_last table_exports { name; id }
  | Mem m -> Dynarray.add_last mem (Origin.Local m)
  | Import { typ = Mem (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    Dynarray.add_last mem imported
  | Export { name; typ = Mem id } ->
    let id = curr_id (Dynarray.length mem) id in
    Dynarray.add_last mem_exports { name; id }
  | Func f -> add_func (Origin.Local f) function_type type_checks func
  | Import { typ = Func (assigned_name, typ); modul_name; name } ->
    let imported = Origin.imported ~modul_name ~name ~assigned_name ~typ in
    add_func imported function_type type_checks func
  | Export { name; typ = Func id } ->
    let id = curr_id (Dynarray.length func) id in
    Dynarray.add_last func_exports { name; id }
  | Elem e ->
    let mode =
      match e.mode with
      | (Text.Elem.Mode.Passive | Declarative) as mode -> mode
      | Active (id, expr) ->
        let id = curr_id (Dynarray.length table) id in
        Active (Some id, expr)
    in
    Dynarray.add_last elem { e with mode }
  | Data d ->
    let mode =
      match d.mode with
      | Passive -> Text.Data.Mode.Passive
      | Active (id, expr) ->
        let id = curr_id (Dynarray.length mem) id in
        Active (Some id, expr)
    in
    Dynarray.add_last data { d with mode }
  | Start id -> start := Some id

let of_text { Text.Module.fields; id } =
  Log.debug (fun m -> m "grouping     ...");
  let typ = Dynarray.create () in
  let function_type = Dynarray.create () in
  let type_checks = Dynarray.create () in
  let global = Dynarray.create () in
  let table = Dynarray.create () in
  let mem = Dynarray.create () in
  let func = Dynarray.create () in
  let elem = Dynarray.create () in
  let data = Dynarray.create () in
  let global_exports = Dynarray.create () in
  let mem_exports = Dynarray.create () in
  let table_exports = Dynarray.create () in
  let func_exports = Dynarray.create () in
  let start = ref None in
  List.iter
    (add_field typ function_type type_checks global table mem func elem data
       global_exports mem_exports table_exports func_exports start )
    fields;
  let modul =
    { id
    ; typ = Dynarray.to_array typ
    ; function_type = Dynarray.to_array function_type
    ; type_checks = Dynarray.to_array type_checks
    ; global = Dynarray.to_array global
    ; table = Dynarray.to_array table
    ; mem = Dynarray.to_array mem
    ; func = Dynarray.to_array func
    ; elem = Dynarray.to_array elem
    ; data = Dynarray.to_array data
    ; global_exports = Dynarray.to_array global_exports
    ; mem_exports = Dynarray.to_array mem_exports
    ; table_exports = Dynarray.to_array table_exports
    ; func_exports = Dynarray.to_array func_exports
    ; start = !start
    }
  in
  Log.debug (fun m -> m "%a" pp modul);
  modul
