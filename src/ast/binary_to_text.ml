(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Binary
open Text
open Types

let convert_indice (t : binary indice) : text indice =
  match t with Raw _ as t -> t

let convert_heap_type (t : binary heap_type) : text heap_type =
  match t with
  | ( Any_ht | None_ht | Eq_ht | I31_ht | Struct_ht | Array_ht | Func_ht
    | No_func_ht | Extern_ht | No_extern_ht ) as t ->
    t
  | Def_ht id -> Def_ht (convert_indice id)

let convert_ref_type (t : binary ref_type) : text ref_type =
  let nullable, heap_type = t in
  (nullable, convert_heap_type heap_type)

let convert_val_type (t : binary val_type) : text val_type =
  match t with
  | Num_type _ as t -> t
  | Ref_type t -> Ref_type (convert_ref_type t)

let convert_global_type (t : binary global_type) : text global_type =
  let mut, vt = t in
  (mut, convert_val_type vt)

let convert_param (p : binary param) : text param =
  let id, vt = p in
  (id, convert_val_type vt)

let convert_param_type (pt : binary param_type) : text param_type =
  List.map convert_param pt

let convert_result_type (rt : binary result_type) : text result_type =
  List.map convert_val_type rt

let convert_func_type ((pt, rt) : binary func_type) : text func_type =
  (convert_param_type pt, convert_result_type rt)

let convert_block_type (bt : binary block_type) : text block_type =
  match bt with
  | Bt_raw (opt, ft) ->
    let opt =
      match opt with None -> None | Some i -> Some (convert_indice i)
    in
    let ft = convert_func_type ft in
    Bt_raw (opt, ft)

let convert_storage_type (t : binary storage_type) : text storage_type =
  match t with
  | Val_storage_t vt -> Val_storage_t (convert_val_type vt)
  | Val_packed_t _ as t -> t

let convert_field_type ((m, t) : binary field_type) : text field_type =
  (m, convert_storage_type t)

let convert_struct_field ((name, field_types) : binary struct_field) :
  text struct_field =
  (name, List.map convert_field_type field_types)

let convert_struct_type (t : binary struct_type) : text struct_type =
  List.map convert_struct_field t

let convert_str_type (str_t : binary str_type) : text str_type =
  match str_t with
  | Def_struct_t t -> Def_struct_t (convert_struct_type t)
  | Def_array_t t -> Def_array_t (convert_field_type t)
  | Def_func_t t -> Def_func_t (convert_func_type t)

let convert_sub_type ((final, indices, str_type) : binary sub_type) :
  text sub_type =
  (final, List.map convert_indice indices, convert_str_type str_type)

let convert_type_def ((name, sub_type) : binary type_def) : text type_def =
  (name, convert_sub_type sub_type)

let convert_rec_type (t : binary rec_type) : text rec_type =
  List.map convert_type_def t

let convert_expr (e : binary expr) : text expr =
  (* TODO: proper conversion ! *)
  Obj.magic e

let convert_table_type (t : binary table_type) : text table_type =
  let limits, t = t in
  (limits, convert_ref_type t)

let convert_table (t : binary table) : text table =
  let id, t = t in
  (id, convert_table_type t)

let convert_elem_mode (e : Binary.elem_mode) : Text.elem_mode =
  match e with
  | Elem_passive -> Elem_passive
  | Elem_declarative -> Elem_declarative
  | Elem_active (opt, e) ->
    let opt = Option.map (fun i -> Raw i) opt in
    let e = convert_expr e in
    Elem_active (opt, e)

let convert_elem (e : Binary.elem) : Text.elem =
  let { Binary.id; typ; init; mode } = e in
  let typ = convert_ref_type typ in
  let init = List.map convert_expr init in
  let mode = convert_elem_mode mode in
  { id; typ; init; mode }

let convert_data_mode (m : Binary.data_mode) : Text.data_mode =
  match m with
  | Data_passive -> Data_passive
  | Data_active (i, e) ->
    let i = Option.map (fun i -> Raw i) i in
    let e = convert_expr e in
    Data_active (i, e)

let convert_data (e : Binary.data) : Text.data =
  let { Binary.id; init; mode } : Binary.data = e in
  let mode = convert_data_mode mode in
  { id; init; mode }

let from_types (types : Types.binary Types.rec_type Named.t) :
  Text.module_field list =
  Named.fold
    (fun i (t : Types.binary Types.rec_type) acc ->
      let t = convert_rec_type t in
      (i, MType t) :: acc )
    types []
  |> List.sort compare |> List.map snd

let from_global (global : (Binary.global, binary global_type) Runtime.t Named.t)
  : Text.module_field list =
  Named.fold
    (fun i (g : (Binary.global, binary global_type) Runtime.t) acc ->
      match g with
      | Runtime.Local g ->
        let typ = convert_global_type g.typ in
        let init = convert_expr g.init in
        let id = g.id in
        (i, MGlobal { typ; init; id }) :: acc
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_global (assigned_name, convert_global_type desc) in
        (i, MImport { modul; name; desc }) :: acc )
    global []
  |> List.sort compare |> List.map snd

let from_table (table : (binary table, binary table_type) Runtime.t Named.t) :
  Text.module_field list =
  Named.fold
    (fun i (t : (binary table, binary table_type) Runtime.t) acc ->
      match t with
      | Runtime.Local t ->
        let t = convert_table t in
        (i, MTable t) :: acc
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_table (assigned_name, convert_table_type desc) in
        (i, MImport { modul; name; desc }) :: acc )
    table []
  |> List.sort compare |> List.map snd

let from_mem (mem : (mem, limits) Runtime.t Named.t) : Text.module_field list =
  Named.fold
    (fun i mem acc ->
      match mem with
      | Runtime.Local mem -> (i, MMem mem) :: acc
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_mem (assigned_name, desc) in
        (i, MImport { modul; name; desc }) :: acc )
    mem []
  |> List.sort compare |> List.map snd

let from_func (func : (binary func, binary block_type) Runtime.t Named.t) :
  Text.module_field list =
  Named.fold
    (fun i (func : (binary func, binary block_type) Runtime.t) acc ->
      match func with
      | Runtime.Local func ->
        let type_f = convert_block_type func.type_f in
        let locals = convert_param_type func.locals in
        let body = convert_expr func.body in
        let id = func.id in
        (i, MFunc { type_f; locals; body; id }) :: acc
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_func (assigned_name, convert_block_type desc) in
        (i, MImport { modul; name; desc }) :: acc )
    func []
  |> List.sort compare |> List.map snd

let from_elem (elem : Binary.elem Named.t) : Text.module_field list =
  Named.fold
    (fun i (elem : Binary.elem) acc ->
      let elem = convert_elem elem in
      (i, MElem elem) :: acc )
    elem []
  |> List.sort compare |> List.map snd

let from_data (data : Binary.data Named.t) : Text.module_field list =
  Named.fold
    (fun i (data : Binary.data) acc ->
      let data = convert_data data in
      (i, MData data) :: acc )
    data []
  |> List.sort compare |> List.map snd

let from_exports (exports : Binary.exports) : Text.module_field list =
  let global =
    List.map
      (fun { name; id } ->
        let id = Some (Raw id) in
        MExport { name; desc = Export_global id } )
      exports.global
  in

  let mem =
    List.map
      (fun { name; id } ->
        let id = Some (Raw id) in
        MExport { name; desc = Export_mem id } )
      exports.mem
  in

  let table =
    List.map
      (fun { name; id } ->
        let id = Some (Raw id) in
        MExport { name; desc = Export_table id } )
      exports.table
  in

  let func =
    List.map
      (fun { name; id } ->
        let id = Some (Raw id) in
        MExport { name; desc = Export_func id } )
      exports.func
  in

  global @ mem @ table @ func

let from_start = function None -> [] | Some n -> [ MStart (Raw n) ]

let modul
  { Binary.id; types; global; table; mem; func; elem; data; start; exports } =
  ignore types;
  let fields =
    from_types types @ from_global global @ from_table table @ from_mem mem
    @ from_func func @ from_elem elem @ from_data data @ from_exports exports
    @ from_start start
  in
  let imported, locals =
    List.partition_map
      (function
        | MImport _ as import -> Either.Left import
        | local -> Either.Right local )
      fields
  in
  let fields = imported @ locals in

  { Text.id; fields }
