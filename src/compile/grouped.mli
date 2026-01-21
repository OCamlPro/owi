(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Text

type opt_export = private
  { name : string
  ; id : indice
  }

type t = private
  { id : string option
  ; typ : Typedef.t Array.t
  ; function_type : func_type Array.t
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : (indice * func_type) Array.t
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Global.t, Global.Type.t) Origin.t Array.t
  ; table : (Table.t, Table.Type.t) Origin.t Array.t
  ; mem : (Mem.t, limits) Origin.t Array.t
  ; func : (Func.t, block_type) Origin.t Array.t
  ; elem : Elem.t Array.t
  ; data : Data.t Array.t
  ; global_exports : opt_export Array.t
  ; mem_exports : opt_export Array.t
  ; table_exports : opt_export Array.t
  ; func_exports : opt_export Array.t
  ; start : indice option
  }

val of_text : Module.t -> t

val pp : t Fmt.t
