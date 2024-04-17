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

open Format
open Types

let symbolic v = Text v

let raw v = Raw v

let bt_ind i = Bt_ind i

let bt_raw i t = Bt_raw (i, t)

type global =
  { typ : text global_type
  ; init : text expr
  ; id : string option
  }

let pp_global fmt (g : global) =
  pp fmt "(global%a %a %a)" pp_id_opt g.id pp_global_type g.typ pp_expr g.init

type data_mode =
  | Data_passive
  | Data_active of text indice option * text expr

let pp_data_mode fmt = function
  | Data_passive -> ()
  | Data_active (i, e) ->
    pp fmt "(memory %a) (offset %a)" pp_indice_opt i pp_expr e

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

let pp_data fmt (d : data) =
  pp fmt {|(data%a %a %S)|} pp_id_opt d.id pp_data_mode d.mode d.init

type elem_mode =
  | Elem_passive
  | Elem_active of text indice option * text expr
  | Elem_declarative

let pp_elem_mode fmt = function
  | Elem_passive -> ()
  | Elem_declarative -> pp fmt "declare"
  | Elem_active (i, e) -> (
    match i with
    | None -> pp fmt "(offset %a)" pp_expr e
    | Some i -> pp fmt "(table %a) (offset %a)" pp_indice i pp_expr e )

type elem =
  { id : string option
  ; typ : text ref_type
  ; init : text expr list
  ; mode : elem_mode
  }

let pp_elem_expr fmt e = pp fmt "(item %a)" pp_expr e

let pp_elem fmt (e : elem) =
  pp fmt "@[<hov 2>(elem%a %a %a %a)@]" pp_id_opt e.id pp_elem_mode e.mode
    pp_ref_type e.typ
    (pp_list ~pp_sep:pp_newline pp_elem_expr)
    e.init

type module_field =
  | MType of text rec_type
  | MGlobal of global
  | MTable of text table
  | MMem of mem
  | MFunc of text func
  | MElem of elem
  | MData of data
  | MStart of text indice
  | MImport of text import
  | MExport of text export

let pp_module_field fmt = function
  | MType t -> pp_rec_type fmt t
  | MGlobal g -> pp_global fmt g
  | MTable t -> pp_table fmt t
  | MMem m -> pp_mem fmt m
  | MFunc f -> pp_func fmt f
  | MElem e -> pp_elem fmt e
  | MData d -> pp_data fmt d
  | MStart s -> pp_start fmt s
  | MImport i -> pp_import fmt i
  | MExport e -> pp_export fmt e

type modul =
  { id : string option
  ; fields : module_field list
  }

let pp_modul fmt (m : modul) =
  pp fmt "(module%a@\n  @[<v>%a@]@\n)" pp_id_opt m.id
    (pp_list ~pp_sep:pp_newline pp_module_field)
    m.fields

type action =
  | Invoke of string option * string * text const list
  | Get of string option * string

let pp_action fmt = function
  | Invoke (mod_name, name, c) ->
    pp fmt {|(invoke%a "%s" %a)|} pp_id_opt mod_name name pp_consts c
  | Get _ -> pp fmt "<action_get TODO>"

type result_const =
  | Literal of text const
  | Nan_canon of nn
  | Nan_arith of nn

let pp_result_const fmt = function
  | Literal c -> pp_const fmt c
  | Nan_canon n -> pp fmt "float%a.const nan:canonical" pp_nn n
  | Nan_arith n -> pp fmt "float%a.const nan:arithmetic" pp_nn n

type result =
  | Result_const of result_const
  | Result_extern_ref
  | Result_func_ref

let pp_result fmt = function
  | Result_const c -> pp fmt "(%a)" pp_result_const c
  | Result_func_ref | Result_extern_ref -> Log.err "not yet implemented"

let pp_result_bis fmt = function
  | Result_const c -> pp fmt "%a" pp_result_const c
  | Result_extern_ref | Result_func_ref -> Log.err "not yet implemented"

let pp_results fmt r = pp_list ~pp_sep:pp_space pp_result_bis fmt r

type assertion =
  | Assert_return of action * result list
  | Assert_trap of action * string
  | Assert_trap_module of modul * string
  | Assert_malformed of modul * string
  | Assert_malformed_quote of string list * string
  | Assert_malformed_binary of string list * string
  | Assert_invalid of modul * string
  | Assert_invalid_quote of string list * string
  | Assert_invalid_binary of string list * string
  | Assert_exhaustion of action * string
  | Assert_unlinkable of modul * string

let pp_strings fmt l = pp fmt "[%a]" (pp_list ~pp_sep:pp_space pp_string) l

let pp_assertion fmt = function
  | Assert_return (a, l) ->
    pp fmt "(assert_return %a %a)" pp_action a pp_results l
  | Assert_exhaustion (a, msg) ->
    pp fmt "(assert_exhaustion %a %s)" pp_action a msg
  | Assert_trap (a, f) -> pp fmt {|(assert_trap %a "%s")|} pp_action a f
  | Assert_trap_module (m, f) ->
    pp fmt {|(assert_trap_module %a "%s")|} pp_modul m f
  | Assert_invalid (m, msg) ->
    pp fmt "(assert_invalid@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_modul m msg
  | Assert_unlinkable (m, msg) ->
    pp fmt "(assert_unlinkable@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_modul m msg
  | Assert_malformed (m, msg) ->
    pp fmt "(assert_malformed@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_modul m msg
  | Assert_malformed_quote (ls, msg) ->
    pp fmt "(assert_malformed_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_strings
      ls msg
  | Assert_invalid_quote (ls, msg) ->
    pp fmt "(assert_invalid_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_strings ls
      msg
  | Assert_malformed_binary (ls, msg) ->
    pp fmt "(assert_malformed_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_strings
      ls msg
  | Assert_invalid_binary (ls, msg) ->
    pp fmt "(assert_invalid_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_strings
      ls msg

type register = string * string option

let pp_register fmt (s, _name) = pp fmt "(register %s)" s

type cmd =
  | Module of modul
  | Assert of assertion
  | Register of string * string option
  | Action of action

let pp_cmd fmt = function
  | Module m -> pp_modul fmt m
  | Assert a -> pp_assertion fmt a
  | Register (s, name) -> pp_register fmt (s, name)
  | Action _a -> pp fmt "<action>"

type script = cmd list

let pp_script fmt l = pp_list ~pp_sep:pp_newline pp_cmd fmt l
