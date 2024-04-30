(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type tbl = (string, int) Hashtbl.t Option.t

val equal_func_types : binary func_type -> binary func_type -> bool

val convert_val_type : tbl -> text val_type -> binary val_type Result.t

val convert_heap_type : tbl -> text heap_type -> binary heap_type Result.t

val convert_func_type : tbl -> text func_type -> binary func_type Result.t

val convert_ref_type : tbl -> text ref_type -> binary ref_type Result.t

val convert_param : tbl -> text param -> binary param Result.t

val convert_table_type : tbl -> text table_type -> binary table_type Result.t

val convert_str : tbl -> text str_type -> binary str_type Result.t
