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
  ; typ : Typedef.t Dynarray.t
  ; function_type : func_type Dynarray.t
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check Dynarray.t
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Global.t, Global.Type.t) Runtime.t Dynarray.t
  ; table : (Table.t, Table.Type.t) Runtime.t Dynarray.t
  ; mem : (Mem.t, limits) Runtime.t Dynarray.t
  ; func : (Func.t, block_type) Runtime.t Dynarray.t
  ; elem : Elem.t Dynarray.t
  ; data : Data.t Dynarray.t
  ; exports : opt_exports
  ; start : indice option
  }

val of_text : Module.t -> t

val pp : Format.formatter -> t -> unit
