(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = private
  { id : string option
  ; typ : Text.func_type Array.t
  ; global : (Text.Global.t, Text.Global.Type.t) Origin.t Array.t
  ; table : (Text.Table.t, Text.Table.Type.t) Origin.t Array.t
  ; mem : (Text.Mem.t, Text.limits) Origin.t Array.t
  ; func : (Text.Func.t, Text.block_type) Origin.t Array.t
  ; elem : Text.Elem.t Array.t
  ; data : Text.Data.t Array.t
  ; typ_names : (string, int) Hashtbl.t
  ; global_names : (string, int) Hashtbl.t
  ; table_names : (string, int) Hashtbl.t
  ; mem_names : (string, int) Hashtbl.t
  ; func_names : (string, int) Hashtbl.t
  ; elem_names : (string, int) Hashtbl.t
  ; data_names : (string, int) Hashtbl.t
  ; global_exports : Grouped.opt_export Array.t
  ; mem_exports : Grouped.opt_export Array.t
  ; table_exports : Grouped.opt_export Array.t
  ; func_exports : Grouped.opt_export Array.t
  ; start : Text.indice option
  }

val of_grouped : Grouped.t -> t Result.t

val find_func : t -> Text.indice -> Binary.indice Result.t

val find_global : t -> Text.indice -> Binary.indice Result.t

val find_memory : t -> Text.indice -> Binary.indice Result.t

val find_data : t -> Text.indice -> Binary.indice Result.t

val find_table : t -> Text.indice -> Binary.indice Result.t

val find_elem : t -> Text.indice -> Binary.indice Result.t

val find_type : t -> Text.indice -> Binary.indice Result.t

val pp : Format.formatter -> t -> unit
