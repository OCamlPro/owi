(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

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

type data_mode =
  | Data_passive
  | Data_active of text indice option * text expr

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

type elem_mode =
  | Elem_passive
  | Elem_active of text indice option * text expr
  | Elem_declarative

type elem =
  { id : string option
  ; typ : text ref_type
  ; init : text expr list
  ; mode : elem_mode
  }

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

type modul =
  { id : string option
  ; fields : module_field list
  }

type action =
  | Invoke of string option * string * text const list
  | Get of string option * string

type result_const =
  | Literal of text const
  | Nan_canon of nn
  | Nan_arith of nn

type result =
  | Result_const of result_const
  | Result_extern_ref
  | Result_func_ref

type assert_ =
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

type cmd =
  | Module of modul
  | Assert of assert_
  | Register of string * string option
  | Action of action

type script = cmd list

module Pp = struct
  include Pp

  let global fmt (g : global) =
    pp fmt "(global %a %a %a)" pp_id_opt g.id global_type g.typ expr g.init

  let symb_indice_opt fmt = function None -> () | Some i -> pp_indice fmt i

  let export_desc fmt = function
    | Export_func id -> pp fmt "(func %a)" symb_indice_opt id
    | Export_table id -> pp fmt "(table %a)" symb_indice_opt id
    | Export_mem id -> pp fmt "(memory %a)" symb_indice_opt id
    | Export_global id -> pp fmt "(global %a)" symb_indice_opt id

  let export fmt (e : text export) =
    pp fmt "(export %a %a)" pp_string e.name export_desc e.desc

  let elem_mode fmt = function
    | Elem_passive -> ()
    | Elem_declarative -> pp fmt "declare"
    | Elem_active (i, e) -> (
      match i with
      | None -> pp fmt "(offset %a)" expr e
      | Some i -> pp fmt "(table %a) (offset %a)" pp_indice i expr e )

  let elemexpr fmt e = pp fmt "(item %a)" expr e

  let data_mode fmt = function
    | Data_passive -> ()
    | Data_active (i, e) ->
      pp fmt "(memory %a) (offset %a)" symb_indice_opt i expr e

  let data fmt (d : data) =
    pp fmt {|(data %a %a %S)|} pp_id_opt d.id data_mode d.mode d.init

  let elem fmt (e : elem) =
    pp fmt "@[<hov 2>(elem %a %a %a %a)@]" pp_id_opt e.id elem_mode e.mode
      pp_ref_type e.typ
      (pp_list ~pp_sep:(fun fmt () -> pp fmt "@\n") elemexpr)
      e.init

  let module_field fmt = function
    | MType t -> typ fmt t
    | MGlobal g -> global fmt g
    | MTable t -> table fmt t
    | MMem m -> mem fmt m
    | MFunc f -> func fmt f
    | MElem e -> elem fmt e
    | MData d -> data fmt d
    | MStart s -> start fmt s
    | MImport i -> import fmt i
    | MExport e -> export fmt e

  let modul fmt (m : modul) =
    pp fmt "(module %a@\n  @[<v>%a@]@\n)" pp_id_opt m.id
      (pp_list ~pp_sep:(fun fmt () -> pp fmt "@\n") module_field)
      m.fields

  let register fmt (s, _name) = pp fmt "(register %s)" s

  let const fmt = function
    | Const_I32 i -> pp fmt "i32.const %ld" i
    | Const_I64 i -> pp fmt "i64.const %Ld" i
    | Const_F32 f -> pp fmt "f32.const %a" Float32.pp f
    | Const_F64 f -> pp fmt "f64.const %a" Float64.pp f
    | Const_null rt -> pp fmt "ref.null %a" pp_heap_type rt
    | Const_host i -> pp fmt "ref.host %d" i
    | Const_extern i -> pp fmt "ref.extern %d" i
    | Const_array -> pp fmt "ref.array"
    | Const_eq -> pp fmt "ref.eq"
    | Const_i31 -> pp fmt "ref.i31"
    | Const_struct -> pp fmt "ref.struct"

  let consts fmt c =
    pp_list ~pp_sep:pp_space (fun fmt c -> pp fmt "(%a)" const c) fmt c

  let action fmt = function
    | Invoke (mod_name, name, c) ->
      pp fmt "(invoke %a %s %a)" pp_id_opt mod_name name consts c
    | Get _ -> pp fmt "<action_get TODO>"

  let result_const fmt = function
    | Literal c -> const fmt c
    | Nan_canon n -> pp fmt "float%a.const nan:canonical" pp_nn n
    | Nan_arith n -> pp fmt "float%a.const nan:arithmetic" pp_nn n

  let result fmt = function
    | Result_const c -> pp fmt "(%a)" result_const c
    | Result_func_ref | Result_extern_ref -> Log.err "not yet implemented"

  let result_bis fmt = function
    | Result_const c -> pp fmt "%a" result_const c
    | Result_extern_ref | Result_func_ref -> Log.err "not yet implemented"

  let results fmt r = pp_list ~pp_sep:pp_space result_bis fmt r

  let strings fmt l = pp fmt "[%a]" (pp_list ~pp_sep:pp_space pp_string) l

  let assert_ fmt = function
    | Assert_return (a, l) -> pp fmt "(assert_return %a %a)" action a results l
    | Assert_exhaustion (a, msg) ->
      pp fmt "(assert_exhaustion %a %s)" action a msg
    | Assert_trap (a, f) -> pp fmt {|(assert_trap %a "%s")|} action a f
    | Assert_trap_module (m, f) ->
      pp fmt {|(assert_trap_module %a "%s")|} modul m f
    | Assert_invalid (m, msg) ->
      pp fmt "(assert_invalid@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" modul m msg
    | Assert_unlinkable (m, msg) ->
      pp fmt "(assert_unlinkable@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" modul m msg
    | Assert_malformed (m, msg) ->
      pp fmt "(assert_malformed@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" modul m msg
    | Assert_malformed_quote (ls, msg) ->
      pp fmt "(assert_malformed_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings
        ls msg
    | Assert_invalid_quote (ls, msg) ->
      pp fmt "(assert_invalid_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls
        msg
    | Assert_malformed_binary (ls, msg) ->
      pp fmt "(assert_malformed_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings
        ls msg
    | Assert_invalid_binary (ls, msg) ->
      pp fmt "(assert_invalid_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls
        msg

  let cmd fmt = function
    | Module m -> modul fmt m
    | Assert a -> assert_ fmt a
    | Register (s, name) -> register fmt (s, name)
    | Action _a -> pp fmt "<action>"

  let file fmt l = pp_list ~pp_sep:(fun fmt () -> pp fmt "@\n") cmd fmt l
end
