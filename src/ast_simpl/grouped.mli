(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*  Written by Léo Andrès and Pierre Chambart                                *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

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
  ; global : (Text.global, simplified global_type) Runtime.t Indexed.t list
  ; table : (simplified table, simplified table_type) Runtime.t Indexed.t list
  ; mem : (mem, Types.limits) Runtime.t Indexed.t list
  ; func : (text func, text block_type) Runtime.t Indexed.t list
  ; elem : Text.elem Indexed.t list
  ; data : Text.data Indexed.t list
  ; exports : opt_exports
  ; start : text indice option
  }

val of_symbolic : Text.modul -> t Result.t
