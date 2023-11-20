(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types
open Syntax
module IMap = Map.Make (Int)

type data = { mutable value : string }

let drop_data data = data.value <- ""

type elem = { mutable value : Concrete_value.ref_value array }

let drop_elem (elem : elem) = elem.value <- [||]

type extern_funcs = Concrete_value.Func.extern_func Func_id.collection

type t' = Env_id.t

type 'ext t =
  { globals : Concrete_global.t IMap.t
  ; memories : Concrete_memory.t IMap.t
  ; tables : Concrete_table.t IMap.t
  ; functions : Func_intf.t IMap.t
  ; data : data IMap.t
  ; elem : elem IMap.t
  ; extern_funcs : 'ext Func_id.collection
  ; id : Env_id.t
  }

let pp fmt t =
  let global fmt (id, (global : Concrete_global.t)) =
    Format.pp fmt "%a -> %a" Format.pp_int id Concrete_value.pp global.value
  in
  let func fmt (id, (_func : Concrete_value.Func.t)) =
    Format.pp fmt "%a -> func" Format.pp_print_int id
  in
  Format.fprintf fmt "@[<hov 2>{@ (globals %a)@ (functions %a)@ }@]"
    (Format.pp_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") global)
    (IMap.bindings t.globals)
    (Format.pp_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") func)
    (IMap.bindings t.functions)

let id (env : _ t) = env.id

let get_global (env : _ t) id = IMap.find id env.globals

let get_memory (env : _ t) id = IMap.find id env.memories

let get_table (env : _ t) id = IMap.find id env.tables

let get_func (env : _ t) id = IMap.find id env.functions

let get_data (env : _ t) id = IMap.find id env.data

let get_elem (env : _ t) id = IMap.find id env.elem

let get_extern_func env id = Func_id.get id env.extern_funcs

let get_func_typ env f =
  match f with
  | Func_intf.WASM (_, func, _) ->
    let (Bt_raw ((None | Some _), t)) = func.type_f in
    t
  | Extern id -> Func_id.get_typ id env.extern_funcs

module Build = struct
  type t =
    { globals : Concrete_global.t IMap.t
    ; memories : Concrete_memory.t IMap.t
    ; tables : Concrete_table.t IMap.t
    ; functions : Func_intf.t IMap.t
    ; data : data IMap.t
    ; elem : elem IMap.t
    }

  let empty =
    { globals = IMap.empty
    ; memories = IMap.empty
    ; tables = IMap.empty
    ; functions = IMap.empty
    ; data = IMap.empty
    ; elem = IMap.empty
    }

  let add_global id const (env : t) =
    { env with globals = IMap.add id const env.globals }

  let add_memory id mem (env : t) =
    { env with memories = IMap.add id mem env.memories }

  let add_table id table (env : t) =
    { env with tables = IMap.add id table env.tables }

  let add_func id func (env : t) =
    { env with functions = IMap.add id func env.functions }

  let add_data id data (env : t) = { env with data = IMap.add id data env.data }

  let add_elem id elem (env : t) = { env with elem = IMap.add id elem env.elem }

  let get_global (env : t) id =
    match IMap.find_opt id env.globals with
    | None ->
      (* Log.debug "%a@." pp env; *)
      Error "unknown global"
    | Some v -> Ok v

  let get_const_global (env : t) id =
    let* g = get_global env id in
    match g.mut with
    | Const -> ok g.value
    | Var -> Error "constant expression required"

  let get_func (env : t) id =
    match IMap.find_opt id env.functions with
    | None ->
      (* Log.debug "%a@." pp env; *)
      error_s "unknown function %a" Format.pp_int id
    | Some v -> Ok v
end

module type T = sig
  module V : Intf.V

  type extern_func

  type t

  type t' = t Lazy.t

  type elem = { mutable value : Concrete_value.ref_value array }

  type data = { mutable value : string }

  type func := Func_intf.t

  val get_memory : t -> int -> Concrete_memory.t Result.t

  val get_func : t -> int -> func Result.t

  val get_table : t -> int -> Concrete_table.t Result.t

  val get_elem : t -> int -> elem Result.t

  val get_data : t -> int -> data Result.t

  val get_global : t -> int -> Concrete_global.t Result.t

  val drop_elem : elem -> unit

  val drop_data : data -> unit

  val get_extern_func : t -> Func_id.t -> Concrete_value.Func.extern_func

  val get_func_typ : t -> func -> simplified func_type

  val pp : Format.formatter -> t -> unit

  val freeze : Build.t -> extern_func Func_id.collection -> t
end

module type P = sig
  module V : Intf.V

  val const_i32 : Int32.t -> V.int32

  val const_i64 : Int64.t -> V.int64

  val const_f32 : Float32.t -> V.float32

  val const_f64 : Float64.t -> V.float64
end

let freeze id ({ globals; memories; tables; functions; data; elem } : Build.t)
  extern_funcs =
  { id; globals; memories; tables; functions; data; elem; extern_funcs }
