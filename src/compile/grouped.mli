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
  ; typ : Typedef.t Iarray.t
  ; function_type : func_type Iarray.t
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : (indice * func_type) Iarray.t
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Global.t, Global.Type.t) Origin.t Iarray.t
  ; table : (Table.t, Table.Type.t) Origin.t Iarray.t
  ; mem : (Mem.t, limits) Origin.t Iarray.t
  ; func : (Func.t, block_type) Origin.t Iarray.t
  ; elem : Elem.t Iarray.t
  ; data : Data.t Iarray.t
  ; global_exports : opt_export Iarray.t
  ; mem_exports : opt_export Iarray.t
  ; table_exports : opt_export Iarray.t
  ; func_exports : opt_export Iarray.t
  ; start : indice option
  }

val of_text : Module.t -> t

val pp : t Fmt.t
