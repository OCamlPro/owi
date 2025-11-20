(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Text

type opt_export =
  { name : string
  ; id : indice
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

val pp_opt_exports : Format.formatter -> opt_exports -> unit

type type_check = indice * func_type

type t =
  { id : string option
  ; typ : Typedef.t list
  ; function_type : func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Global.t, Global.Type.t) Runtime.t Indexed.t list
  ; table : (Table.t, Table.Type.t) Runtime.t Indexed.t list
  ; mem : (mem, limits) Runtime.t Indexed.t list
  ; func : (Func.t, block_type) Runtime.t Indexed.t list
  ; elem : Elem.t Indexed.t list
  ; data : Data.t Indexed.t list
  ; exports : opt_exports
  ; start : indice option
  }

val of_text : Module.t -> t

val pp : Format.formatter -> t -> unit
