(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax
module IMap = Map.Make (Int)

type data = { mutable value : string }

let drop_data data = data.value <- ""

type elem = { mutable value : Concrete_value.ref_value array }

let drop_elem (elem : elem) = elem.value <- [||]

type extern_funcs = Concrete_extern_func.extern_func Func_id.collection

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

type 'ext backup = 'ext t

let backup_data (data : data) : data = { value = data.value }

let backup_elem (elem : elem) : elem = { value = elem.value }

let recover_data ~(from_ : data) ~(to_ : data) = to_.value <- from_.value

let recover_elem ~(from_ : elem) ~(to_ : elem) = to_.value <- from_.value

let backup t =
  { t with
    globals = IMap.map Concrete_global.backup t.globals
  ; memories = IMap.map Concrete_memory.backup t.memories
  ; tables = IMap.map Concrete_table.backup t.tables
  ; data = IMap.map backup_data t.data
  ; elem = IMap.map backup_elem t.elem
  }

let recover backup into =
  let apply f _key v1 v2 =
    match (v1, v2) with
    | Some v1, Some v2 ->
      f ~from_:v1 ~to_:v2;
      None
    | _ -> assert false
  in
  let _ : _ IMap.t =
    IMap.merge (apply Concrete_global.recover) backup.globals into.globals
  in
  let _ : _ IMap.t =
    IMap.merge (apply Concrete_memory.recover) backup.memories into.memories
  in
  let _ : _ IMap.t =
    IMap.merge (apply Concrete_table.recover) backup.tables into.tables
  in
  let _ : _ IMap.t = IMap.merge (apply recover_data) backup.data into.data in
  let _ : _ IMap.t = IMap.merge (apply recover_elem) backup.elem into.elem in
  ()

let id (env : _ t) = env.id

let get_global (env : _ t) id =
  match IMap.find_opt id env.globals with None -> assert false | Some v -> v

let get_memory (env : _ t) id =
  match IMap.find_opt id env.memories with None -> assert false | Some v -> v

let get_table (env : _ t) id =
  match IMap.find_opt id env.tables with None -> assert false | Some v -> v

let get_func (env : _ t) id =
  match IMap.find_opt id env.functions with None -> assert false | Some v -> v

let get_data (env : _ t) id =
  match IMap.find_opt id env.data with None -> assert false | Some v -> v

let get_elem (env : _ t) id =
  match IMap.find_opt id env.elem with None -> assert false | Some v -> v

let get_extern_func env id =
  match Func_id.get id env.extern_funcs with
  | None -> assert false
  | Some v -> v

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
    | None -> Error (`Unknown_global (Raw id))
    | Some v -> Ok v

  let get_const_global (env : t) id =
    let* g = get_global env id in
    match g.mut with
    | Const -> ok g.value
    | Var -> Error `Constant_expression_required

  let get_func (env : t) id =
    match IMap.find_opt id env.functions with
    | None -> Error (`Unknown_func (Raw id))
    | Some v -> Ok v
end

module type T = sig
  type extern_func

  type t

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

  val get_extern_func : t -> Func_id.t -> Concrete_extern_func.extern_func

  val get_func_typ : t -> func -> binary func_type

  val pp : Format.formatter -> t -> unit

  val freeze : Build.t -> extern_func Func_id.collection -> t
end

module type P = sig
  val const_i32 : Int32.t -> Concrete_value.int32

  val const_i64 : Int64.t -> Concrete_value.int64

  val const_f32 : Float32.t -> Concrete_value.float32

  val const_f64 : Float64.t -> Concrete_value.float64
end

let freeze id ({ globals; memories; tables; functions; data; elem } : Build.t)
  extern_funcs =
  { id; globals; memories; tables; functions; data; elem; extern_funcs }
