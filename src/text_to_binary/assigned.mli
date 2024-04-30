(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type t =
  { id : string option
  ; typ : binary str_type Named.t
  ; global : (Text.global, binary global_type) Runtime.t Named.t
  ; table : (binary table, binary table_type) Runtime.t Named.t
  ; mem : (Types.mem, Types.limits) Runtime.t Named.t
  ; func : (text func, text block_type) Runtime.t Named.t
  ; elem : Text.elem Named.t
  ; data : Text.data Named.t
  ; exports : Grouped.opt_exports
  ; start : text indice option
  }

val of_grouped : Grouped.t -> t Result.t
