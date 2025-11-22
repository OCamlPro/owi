(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Text

type opt_export =
  { name : string
  ; id : indice
  }

type t =
  { id : string option
  ; typ : Typedef.t Dynarray.t
  ; function_type : func_type Dynarray.t
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : (indice * func_type) Dynarray.t
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Global.t, Global.Type.t) Origin.t Dynarray.t
  ; table : (Table.t, Table.Type.t) Origin.t Dynarray.t
  ; mem : (Mem.t, limits) Origin.t Dynarray.t
  ; func : (Func.t, block_type) Origin.t Dynarray.t
  ; elem : Elem.t Dynarray.t
  ; data : Data.t Dynarray.t
  ; global_exports : opt_export Dynarray.t
  ; mem_exports : opt_export Dynarray.t
  ; table_exports : opt_export Dynarray.t
  ; func_exports : opt_export Dynarray.t
  ; mutable start : indice option
  }

val of_text : Module.t -> t

val pp : Format.formatter -> t -> unit
