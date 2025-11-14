(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type opt_export =
  { name : string
  ; id : text indice
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

val pp_opt_exports : Format.formatter -> opt_exports -> unit

type type_check = text indice * func_type

type t =
  { id : string option
  ; typ : type_def list
  ; function_type : func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Text.global, global_type) Runtime.t Indexed.t list
  ; table : (table, table_type) Runtime.t Indexed.t list
  ; mem : (mem, Types.limits) Runtime.t Indexed.t list
  ; func : (text func, text block_type) Runtime.t Indexed.t list
  ; elem : Text.elem Indexed.t list
  ; data : Text.data Indexed.t list
  ; exports : opt_exports
  ; start : text indice option
  ; annots : text Annot.annot list
  }

val of_text : Text.modul -> t

val pp : Format.formatter -> t -> unit
