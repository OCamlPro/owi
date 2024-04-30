(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type opt_ind =
  | Curr of int
  | Indice of text indice

type opt_export =
  { name : string
  ; id : opt_ind
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

type type_check = text indice * text func_type

type t =
  { id : string option
  ; typ : text type_def list
  ; function_type : text func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Text.global, binary global_type) Runtime.t Indexed.t list
  ; table : (binary table, binary table_type) Runtime.t Indexed.t list
  ; mem : (mem, Types.limits) Runtime.t Indexed.t list
  ; func : (text func, text block_type) Runtime.t Indexed.t list
  ; elem : Text.elem Indexed.t list
  ; data : Text.data Indexed.t list
  ; exports : opt_exports
  ; start : text indice option
  }

val of_symbolic : Text.modul -> t Result.t
