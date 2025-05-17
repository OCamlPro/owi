(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type tbl = (string, int) Hashtbl.t Option.t

val convert_val_type : text val_type -> binary val_type Result.t

val convert_heap_type : text heap_type -> binary heap_type Result.t

val convert_func_type : text func_type -> binary func_type Result.t

val convert_ref_type : text ref_type -> binary ref_type Result.t

val convert_param : text param -> binary param Result.t

val convert_table_type : text table_type -> binary table_type Result.t
