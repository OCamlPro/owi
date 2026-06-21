(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

val of_grouped : Grouped.t -> t Result.t

val find_func : t -> Text.indice -> Binary.indice Result.t

val find_global : t -> Text.indice -> Binary.indice Result.t

val find_memory : t -> Text.indice -> Binary.indice Result.t

val find_data : t -> Text.indice -> Binary.indice Result.t

val find_table : t -> Text.indice -> Binary.indice Result.t

val find_elem : t -> Text.indice -> Binary.indice Result.t

val find_type : t -> Text.indice -> Binary.indice Result.t

val find_tag : t -> Text.indice -> Binary.indice Result.t

val get_func_type : t -> int -> Text.func_type Option.t

val get_types : t -> Text.sub_type Array.t

val find_raw_func_type : t -> Text.func_type -> Binary.indice

val find_field : t -> int -> Text.indice -> Binary.indice Result.t

val pp : t Fmt.t
