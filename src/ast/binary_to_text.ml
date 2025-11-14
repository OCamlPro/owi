(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Binary
open Text
open Types

let convert_block_type (bt : binary block_type) : text block_type =
  match bt with
  | Bt_raw (opt, ft) ->
    let opt = match opt with None -> None | Some (Raw _ as opt) -> Some opt in
    Bt_raw (opt, ft)

let convert_expr (e : binary expr Annotated.t) : text expr Annotated.t =
  (* TODO: proper conversion ! *)
  Obj.magic e

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
  let init = List.map convert_expr init in
  let mode = convert_elem_mode mode in
  { id; typ; init; mode }

let convert_data_mode (m : Binary.data_mode) : Text.data_mode =
  match m with
  | Data_passive -> Data_passive
  | Data_active (i, e) ->
    let e = convert_expr e in
    Data_active (Some (Raw i), e)

let convert_data (e : Binary.data) : Text.data =
  let { Binary.id; init; mode } : Binary.data = e in
  let mode = convert_data_mode mode in
  { id; init; mode }

let from_types types : Text.module_field list =
  Array.map (fun (t : Types.type_def) -> MType t) types |> Array.to_list

let from_global global : Text.module_field list =
  Array.map
    (function
      | Runtime.Local (g : Binary.global) ->
        let typ = g.typ in
        let init = convert_expr g.init in
        let id = g.id in
        MGlobal { typ; init; id }
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_global (assigned_name, desc) in
        MImport { modul; name; desc } )
    global
  |> Array.to_list

let from_table table : Text.module_field list =
  Array.map
    (function
      | Runtime.Local t -> MTable t
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_table (assigned_name, desc) in
        MImport { modul; name; desc } )
    table
  |> Array.to_list

let from_mem mem : Text.module_field list =
  Array.map
    (function
      | Runtime.Local mem -> MMem mem
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_mem (assigned_name, desc) in
        MImport { modul; name; desc } )
    mem
  |> Array.to_list

let from_func func : Text.module_field list =
  Array.map
    (function
      | Runtime.Local func ->
        let type_f = convert_block_type func.type_f in
        let locals = func.locals in
        let body = convert_expr func.body in
        let id = func.id in
        MFunc { type_f; locals; body; id }
      | Imported { modul; name; assigned_name; desc } ->
        let desc = Import_func (assigned_name, convert_block_type desc) in
        MImport { modul; name; desc } )
    func
  |> Array.to_list

let from_elem elem : Text.module_field list =
  Array.map
    (fun (elem : Binary.elem) ->
      let elem = convert_elem elem in
      MElem elem )
    elem
  |> Array.to_list

let from_data data : Text.module_field list =
  Array.map
    (fun (data : Binary.data) ->
      let data = convert_data data in
      MData data )
    data
  |> Array.to_list

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
  { Binary.Module.id
  ; types
  ; global
  ; table
  ; mem
  ; func
  ; elem
  ; data
  ; start
  ; exports
  ; _
  } =
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
  let annots = [] in

  { Text.id; fields; annots }
