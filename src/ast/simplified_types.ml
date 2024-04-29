(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax

let equal_func_types (a : simplified func_type) (b : simplified func_type) :
  bool =
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

type tbl = (string, int) Hashtbl.t Option.t

let convert_heap_type tbl = function
  | ( Any_ht | None_ht | Eq_ht | I31_ht | Struct_ht | Array_ht | Func_ht
    | No_func_ht | Extern_ht | No_extern_ht
    | Def_ht (Raw _) ) as t ->
    Ok t
  | Def_ht (Text i) -> begin
    match tbl with
    | None -> Error `Unknown_type
    | Some tbl -> begin
      match Hashtbl.find_opt tbl i with
      | None -> Error `Unknown_type
      | Some i -> ok @@ Def_ht (Raw i)
    end
  end

let convert_ref_type tbl (null, heap_type) =
  let+ heap_type = convert_heap_type tbl heap_type in
  (null, heap_type)

let convert_val_type tbl : text val_type -> simplified val_type Result.t =
  function
  | Num_type _t as t -> Ok t
  | Ref_type rt ->
    let+ rt = convert_ref_type tbl rt in
    Ref_type rt

let convert_param tbl (n, t) =
  let+ t = convert_val_type tbl t in
  (n, t)

let convert_pt tbl l = list_map (convert_param tbl) l

let convert_rt tbl l = list_map (convert_val_type tbl) l

let convert_func_type tbl (pt, rt) =
  let* pt = convert_pt tbl pt in
  let+ rt = convert_rt tbl rt in
  (pt, rt)

let convert_storage_type tbl = function
  | Val_storage_t val_type ->
    let+ val_type = convert_val_type tbl val_type in
    Val_storage_t val_type
  | Val_packed_t _packed_type as t -> Ok t

let convert_field_type tbl (mut, storage_type) =
  let+ storage_type = convert_storage_type tbl storage_type in
  (mut, storage_type)

let convert_struct_field tbl (id, types) =
  let+ types = list_map (convert_field_type tbl) types in
  (id, types)

let convert_struct_type tbl fields = list_map (convert_struct_field tbl) fields

let convert_str tbl = function
  | Def_func_t func_t ->
    let+ func_t = convert_func_type tbl func_t in
    Def_func_t func_t
  | Def_array_t field_t ->
    let+ field_t = convert_field_type tbl field_t in
    Def_array_t field_t
  | Def_struct_t struct_t ->
    let+ struct_t = convert_struct_type tbl struct_t in
    Def_struct_t struct_t

let convert_table_type tbl (limits, ref_type) =
  let+ ref_type = convert_ref_type tbl ref_type in
  (limits, ref_type)
