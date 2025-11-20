(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { id : string option
  ; typ : Text.func_type Named.t
  ; global : (Text.Global.t, Text.Global.Type.t) Runtime.t Named.t
  ; table : (Text.Table.t, Text.Table.Type.t) Runtime.t Named.t
  ; mem : (Text.Mem.t, Text.limits) Runtime.t Named.t
  ; func : (Text.Func.t, Text.block_type) Runtime.t Named.t
  ; elem : Text.Elem.t Named.t
  ; data : Text.Data.t Named.t
  ; exports : Grouped.opt_exports
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
