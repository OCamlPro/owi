(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax

type tbl = (string, int) Hashtbl.t Option.t

let convert_heap_type = function (Func_ht | Extern_ht) as t -> Ok t

let convert_ref_type (null, heap_type) =
  let+ heap_type = convert_heap_type heap_type in
  (null, heap_type)

let convert_val_type : text val_type -> binary val_type Result.t = function
  | Num_type _t as t -> Ok t
  | Ref_type rt ->
    let+ rt = convert_ref_type rt in
    Ref_type rt

let convert_param (n, t) =
  let+ t = convert_val_type t in
  (n, t)

let convert_pt l = list_map convert_param l

let convert_rt l = list_map convert_val_type l

let convert_func_type (pt, rt) =
  let* pt = convert_pt pt in
  let+ rt = convert_rt rt in
  (pt, rt)

let convert_table_type (limits, ref_type) =
  let+ ref_type = convert_ref_type ref_type in
  (limits, ref_type)
