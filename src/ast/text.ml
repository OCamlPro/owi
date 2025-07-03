(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt
open Types

let symbolic v = Text v

let raw v = Raw v

let bt_ind i = Bt_ind i

let bt_raw i t = Bt_raw (i, t)

type global =
  { typ : text global_type
  ; init : text expr Annotated.t
  ; id : string option
  }

let pp_global fmt (g : global) =
  pf fmt "(global%a %a %a)" pp_id_opt g.id pp_global_type g.typ
    (pp_expr ~short:false) g.init

type data_mode =
  | Data_passive
  | Data_active of text indice option * text expr Annotated.t

let pp_data_mode fmt = function
  | Data_passive -> ()
  | Data_active (i, e) ->
    pf fmt "(memory %a) (offset %a)" pp_indice_opt i (pp_expr ~short:false) e

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

let pp_data fmt (d : data) =
  pf fmt {|(data%a %a %S)|} pp_id_opt d.id pp_data_mode d.mode d.init

type elem_mode =
  | Elem_passive
  | Elem_active of text indice option * text expr Annotated.t
  | Elem_declarative

let pp_elem_mode fmt = function
  | Elem_passive -> ()
  | Elem_declarative -> pf fmt "declare"
  | Elem_active (i, e) -> (
    match i with
    | None -> pf fmt "(offset %a)" (pp_expr ~short:false) e
    | Some i ->
      pf fmt "(table %a) (offset %a)" pp_indice i (pp_expr ~short:false) e )

type elem =
  { id : string option
  ; typ : text ref_type
  ; init : text expr Annotated.t list
  ; mode : elem_mode
  }

let pp_elem_expr fmt e = pf fmt "(item %a)" (pp_expr ~short:false) e

let pp_elem fmt (e : elem) =
  pf fmt "@[<hov 2>(elem%a %a %a %a)@]" pp_id_opt e.id pp_elem_mode e.mode
    pp_ref_type e.typ
    (list ~sep:pp_newline pp_elem_expr)
    e.init

type module_field =
  | MType of text type_def
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
  | MType t -> pp_type_def fmt t
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
  ; annots : text Annot.annot list
  }

let pp_modul fmt (m : modul) =
  pf fmt "%a(module%a@\n  @[<v>%a@]@\n)"
    (list ~sep:pp_newline Annot.pp_annot)
    m.annots pp_id_opt m.id
    (list ~sep:pp_newline pp_module_field)
    m.fields

type action =
  | Invoke of string option * string * text const list
  | Get of string option * string

let pp_action fmt = function
  | Invoke (mod_name, name, c) ->
    pf fmt {|(invoke%a "%s" %a)|} pp_id_opt mod_name name pp_consts c
  | Get _ -> pf fmt "<action_get TODO>"

type result_const =
  | Literal of text const
  | Nan_canon of nn
  | Nan_arith of nn

let pp_result_const fmt = function
  | Literal c -> pp_const fmt c
  | Nan_canon n -> pf fmt "f%a.const nan:canonical" pp_nn n
  | Nan_arith n -> pf fmt "f%a.const nan:arithmetic" pp_nn n

type result =
  | Result_const of result_const
  | Result_extern_ref
  | Result_func_ref

let pp_result fmt = function
  | Result_const c -> pf fmt "(%a)" pp_result_const c
  | Result_func_ref | Result_extern_ref -> assert false

let pp_result_bis fmt = function
  | Result_const c -> pf fmt "%a" pp_result_const c
  | Result_extern_ref | Result_func_ref -> assert false

let pp_results fmt r = list ~sep:sp pp_result_bis fmt r

type assertion =
  | Assert_return of action * result list
  | Assert_trap of action * string
  | Assert_trap_module of modul * string
  | Assert_malformed of modul * string
  | Assert_malformed_quote of string * string
  | Assert_malformed_binary of string * string
  | Assert_invalid of modul * string
  | Assert_invalid_quote of string * string
  | Assert_invalid_binary of string * string
  | Assert_exhaustion of action * string
  | Assert_unlinkable of modul * string

let pp_assertion fmt = function
  | Assert_return (a, l) ->
    pf fmt "(assert_return %a %a)" pp_action a pp_results l
  | Assert_exhaustion (a, msg) ->
    pf fmt "(assert_exhaustion %a %s)" pp_action a msg
  | Assert_trap (a, f) -> pf fmt {|(assert_trap %a "%s")|} pp_action a f
  | Assert_trap_module (m, f) ->
    pf fmt {|(assert_trap_module %a "%s")|} pp_modul m f
  | Assert_invalid (m, msg) ->
    pf fmt "(assert_invalid@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_modul m msg
  | Assert_unlinkable (m, msg) ->
    pf fmt "(assert_unlinkable@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" pp_modul m msg
  | Assert_malformed (m, msg) ->
    pf fmt "(assert_malformed (module binary@\n  @[<v>%a@])@\n  @[<v>%S@]@\n)"
      pp_modul m msg
  | Assert_malformed_quote (ls, msg) ->
    pf fmt "(assert_malformed_quote@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg
  | Assert_invalid_quote (ls, msg) ->
    pf fmt "(assert_invalid_quote@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg
  | Assert_malformed_binary (ls, msg) ->
    pf fmt "(assert_malformed_binary@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg
  | Assert_invalid_binary (ls, msg) ->
    pf fmt "(assert_invalid_binary@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg

type register = string * string option

let pp_register fmt (s, _name) = pf fmt "(register %s)" s

type cmd =
  | Quoted_module of string
  | Binary_module of string option * string
  | Text_module of modul
  | Assert of assertion
  | Register of string * string option
  | Action of action

let pp_cmd fmt = function
  | Quoted_module m -> pf fmt "(module %S)" m
  | Binary_module (id, m) -> Fmt.pf fmt "(module %a %S)" Types.pp_id_opt id m
  | Text_module m -> pp_modul fmt m
  | Assert a -> pp_assertion fmt a
  | Register (s, name) -> pp_register fmt (s, name)
  | Action _a -> pf fmt "<action>"

type script = cmd list

let pp_script fmt l = list ~sep:pp_newline pp_cmd fmt l
