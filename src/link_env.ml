(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module IMap = Map.Make (Int)

type data = { mutable value : string }

let drop_data data = data.value <- ""

type 'env elem = { mutable value : 'env Value.ref_value array }

let drop_elem elem = elem.value <- [||]

type t =
  { globals : t' Global.t IMap.t
  ; memories : Memory.t IMap.t
  ; tables : t' Table.t IMap.t
  ; functions : t' Value.Func.t IMap.t
  ; data : data IMap.t
  ; elem : t' elem IMap.t
  }

and t' = t lazy_t

let pp fmt t =
  let global fmt (id, (global : 'a Global.t)) =
    Format.fprintf fmt "%a -> %a" Format.pp_print_int id Value.pp global.value
  in
  let func fmt (id, (_func : 'a Value.Func.t)) =
    Format.fprintf fmt "%a -> func" Format.pp_print_int id
  in
  Format.fprintf fmt "@[<hov 2>{@ (globals %a)@ (functions %a)@ }@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       global )
    (IMap.bindings t.globals)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       func )
    (IMap.bindings t.functions)

let get_global (env : t) id =
  match IMap.find_opt id env.globals with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown global"
  | Some v -> Ok v

let get_memory (env : t) id =
  match IMap.find_opt id env.memories with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown memory"
  | Some v -> Ok v

let get_table (env : t) id =
  match IMap.find_opt id env.tables with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown table"
  | Some v -> Ok v

let get_func (env : t) id =
  match IMap.find_opt id env.functions with
  | None ->
    Log.debug "%a@." pp env;
    error_s "unknown function %a" Format.pp_print_int id
  | Some v -> Ok v

let get_data (env : t) id =
  match IMap.find_opt id env.data with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown data"
  | Some v -> Ok v

let get_elem (env : t) id =
  match IMap.find_opt id env.elem with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown elem"
  | Some v -> Ok v

module Build = struct
  type nonrec t = t

  let empty =
    { globals = IMap.empty
    ; memories = IMap.empty
    ; tables = IMap.empty
    ; functions = IMap.empty
    ; data = IMap.empty
    ; elem = IMap.empty
    }

  let add_global id const env =
    { env with globals = IMap.add id const env.globals }

  let add_memory id mem env =
    { env with memories = IMap.add id mem env.memories }

  let add_table id table env =
    { env with tables = IMap.add id table env.tables }

  let add_func id func env =
    { env with functions = IMap.add id func env.functions }

  let add_data id data env = { env with data = IMap.add id data env.data }

  let add_elem id elem env = { env with elem = IMap.add id elem env.elem }
end

let freeze t = t
