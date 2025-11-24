(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = private
  { typ : Text.func_type Array.t
  ; typ_names : (string, int) Hashtbl.t
  ; global_names : (string, int) Hashtbl.t
  ; table_names : (string, int) Hashtbl.t
  ; mem_names : (string, int) Hashtbl.t
  ; func_names : (string, int) Hashtbl.t
  ; elem_names : (string, int) Hashtbl.t
  ; data_names : (string, int) Hashtbl.t
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
