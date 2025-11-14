(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type global = Concrete_global.t

type table = Concrete_table.t

type func = Concrete_extern_func.t

type exports =
  { globals : global StringMap.t
  ; memories : Concrete_memory.t StringMap.t
  ; tables : table StringMap.t
  ; functions : func StringMap.t
  ; defined_names : StringSet.t
  }

type 'f module_to_run =
  { id : string option
  ; env : 'f Link_env.t
  ; to_run : binary expr Annotated.t list
  }

type 'f envs = 'f Link_env.t Env_id.collection

type fenvs = Concrete_extern_func.extern_func Link_env.t Env_id.collection

type 'f state =
  { by_name : exports StringMap.t
  ; by_id : (exports * Env_id.t) StringMap.t
  ; last : (exports * Env_id.t) option
  ; collection : 'f Func_id.collection
  ; envs : 'f envs
  }

type 'extern_func extern_module = { functions : (string * 'extern_func) list }

let empty_state =
  { by_name = StringMap.empty
  ; by_id = StringMap.empty
  ; last = None
  ; collection = Func_id.empty
  ; envs = Env_id.empty
  }

let load_from_module ls f (import : _ Imported.t) =
  match StringMap.find_opt import.modul ls.by_name with
  | None -> Error (`Unknown_module import.modul)
  | Some exports -> (
    match StringMap.find_opt import.name (f exports) with
    | None ->
      if StringSet.mem import.name exports.defined_names then
        Error (`Incompatible_import_type import.name)
      else Error (`Unknown_import (import.modul, import.name))
    | Some v -> Ok v )

let load_global (ls : 'f state) (import : global_type Imported.t) :
  global Result.t =
  let* global = load_from_module ls (fun (e : exports) -> e.globals) import in
  let* () =
    match (fst import.desc, global.mut) with
    | Var, Const | Const, Var -> Error (`Incompatible_import_type import.name)
    | Const, Const | Var, Var -> Ok ()
  in
  if not @@ Types.val_type_eq (snd import.desc) global.typ then begin
    Error (`Incompatible_import_type import.name)
  end
  else Ok global

module Eval_const = struct
  module Stack = Stack.Make [@inlined hint] (Concrete_value)

  (* TODO: const ibinop *)
  let ibinop stack nn (op : ibinop) =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      Stack.push_i32 stack
        (let open Int32 in
         match op with
         | Add -> add n1 n2
         | Sub -> sub n1 n2
         | Mul -> mul n1 n2
         | _ -> assert false )
    | S64 ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      Stack.push_i64 stack
        (let open Int64 in
         match op with
         | Add -> add n1 n2
         | Sub -> sub n1 n2
         | Mul -> mul n1 n2
         | _ -> assert false )

  (* TODO: binary+const instr *)
  let instr env stack instr =
    match instr.Annotated.raw with
    | I32_const n -> ok @@ Stack.push_i32 stack n
    | I64_const n -> ok @@ Stack.push_i64 stack n
    | F32_const f -> ok @@ Stack.push_f32 stack f
    | F64_const f -> ok @@ Stack.push_f64 stack f
    | V128_const f -> ok @@ Stack.push_v128 stack f
    | I_binop (nn, op) -> ok @@ ibinop stack nn op
    | Ref_null t -> ok @@ Stack.push stack (Concrete_value.ref_null t)
    | Ref_func (Raw f) ->
      let* f = Link_env.Build.get_func env f in
      let value = Concrete_value.Ref (Funcref (Some f)) in
      ok @@ Stack.push stack value
    | Global_get (Raw id) ->
      let* g = Link_env.Build.get_const_global env id in
      ok @@ Stack.push stack g
    | _ -> assert false

  (* TODO: binary+const expr *)
  let expr env e : Concrete_value.t Result.t =
    let* stack = list_fold_left (instr env) Stack.empty e.Annotated.raw in
    match stack with
    | [] -> Error (`Type_mismatch "const expr returning zero values")
    | _ :: _ :: _ ->
      Error (`Type_mismatch "const expr returning more than one value")
    | [ result ] -> Ok result
end

let eval_global ls env (global : (Binary.global, global_type) Runtime.t) :
  global Result.t =
  match global with
  | Local global ->
    let* value = Eval_const.expr env global.init in
    let mut, typ = global.typ in
    let global : global = { value; label = global.id; mut; typ } in
    Ok global
  | Imported import -> load_global ls import

let eval_globals ls env globals : Link_env.Build.t Result.t =
  let+ env, _i =
    array_fold_left
      (fun (env, i) global ->
        let+ global = eval_global ls env global in
        let env = Link_env.Build.add_global i global env in
        (env, succ i) )
      (env, 0) globals
  in
  env

(*
let eval_in_data (env : Link_env.t) (data : _ data') : (int, value) data' =
  let mode =
    match data.mode with
    | Data_passive -> Data_passive
    | Data_active (id, expr) ->
      let const = Const_interp.exec_expr env expr in
      Data_active (id, const)
  in
  { data with mode }
*)

let limit_is_included ~import ~imported =
  imported.min >= import.min
  &&
  match (imported.max, import.max) with
  | _, None -> true
  | None, Some _ -> false
  | Some i, Some j -> i <= j

let load_memory (ls : 'f state) (import : limits Imported.t) :
  Concrete_memory.t Result.t =
  let* mem = load_from_module ls (fun (e : exports) -> e.memories) import in
  let imported_limit = Concrete_memory.get_limits mem in
  if limit_is_included ~import:import.desc ~imported:imported_limit then Ok mem
  else Error (`Incompatible_import_type import.name)

let eval_memory ls (memory : (mem, limits) Runtime.t) :
  Concrete_memory.t Result.t =
  match memory with
  | Local (_label, mem_type) -> ok @@ Concrete_memory.init mem_type
  | Imported import -> load_memory ls import

let eval_memories ls env memories =
  let+ env, _i =
    array_fold_left
      (fun (env, id) mem ->
        let+ memory = eval_memory ls mem in
        let env = Link_env.Build.add_memory id memory env in
        (env, succ id) )
      (env, 0) memories
  in
  env

let table_types_are_compatible (import, (t1 : ref_type)) (imported, t2) =
  limit_is_included ~import ~imported && Types.ref_type_eq t1 t2

let load_table (ls : 'f state) (import : table_type Imported.t) : table Result.t
    =
  let typ : table_type = import.desc in
  let* t = load_from_module ls (fun (e : exports) -> e.tables) import in
  if table_types_are_compatible typ (t.limits, t.typ) then Ok t
  else Error (`Incompatible_import_type import.name)

let eval_table ls (table : (_, table_type) Runtime.t) : table Result.t =
  match table with
  | Local (label, table_type) -> ok @@ Concrete_table.init ?label table_type
  | Imported import -> load_table ls import

let eval_tables ls env tables =
  let+ env, _i =
    array_fold_left
      (fun (env, i) table ->
        let+ table = eval_table ls table in
        let env = Link_env.Build.add_table i table env in
        (env, succ i) )
      (env, 0) tables
  in
  env

let load_func (ls : 'f state) (import : binary block_type Imported.t) :
  func Result.t =
  let (Bt_raw ((None | Some _), typ)) = import.desc in
  let* func = load_from_module ls (fun (e : exports) -> e.functions) import in
  let type' =
    match func with
    | Func_intf.WASM (_, func, _) ->
      let (Bt_raw ((None | Some _), t)) = func.type_f in
      t
    | Extern func_id -> Func_id.get_typ func_id ls.collection
  in
  if Types.func_type_eq typ type' then Ok func
  else Error (`Incompatible_import_type import.name)

let eval_func ls (finished_env : Link_env.t') func : func Result.t =
  match func with
  | Runtime.Local func -> ok @@ Concrete_extern_func.wasm func finished_env
  | Imported import -> load_func ls import

let eval_functions ls (finished_env : Link_env.t') env functions =
  let+ env, _i =
    array_fold_left
      (fun (env, i) func ->
        let+ func = eval_func ls finished_env func in
        let env = Link_env.Build.add_func i func env in
        (env, succ i) )
      (env, 0) functions
  in
  env

let active_elem_expr ~offset ~length ~table ~elem =
  [ I32_const offset
  ; I32_const 0l
  ; I32_const length
  ; Table_init (Raw table, Raw elem)
  ; Elem_drop (Raw elem)
  ]

let active_data_expr ~offset ~length ~mem ~data =
  if mem <> 0 then Error (`Unknown_memory (Raw mem))
  else
    Ok
      [ I32_const offset
      ; I32_const 0l
      ; I32_const length
      ; Memory_init data
      ; Data_drop data
      ]

let get_i32 = function
  | Concrete_value.I32 i -> Ok i
  | _ -> Error (`Type_mismatch "get_i32")

let define_data env data =
  let+ env, init, _i =
    array_fold_left
      (fun (env, init, i) (data : Binary.data) ->
        let data' : Link_env.data = { value = data.init } in
        let env = Link_env.Build.add_data i data' env in
        let+ init =
          match data.mode with
          | Data_active (mem, offset) ->
            let* offset = Eval_const.expr env offset in
            let length = Int32.of_int @@ String.length data.init in
            let* offset = get_i32 offset in
            let id = Raw i in
            let* v = active_data_expr ~offset ~length ~mem ~data:id in
            ok @@ (v :: init)
          | Data_passive -> Ok init
        in
        (env, init, succ i) )
      (env, [], 0) data
  in
  (env, List.rev init)

let define_elem env elem =
  let+ env, inits, _i =
    array_fold_left
      (fun (env, inits, i) (elem : Binary.elem) ->
        let* init = list_map (Eval_const.expr env) elem.init in
        let* init_as_ref =
          list_map
            (function
              | Concrete_value.Ref v -> Ok v
              | _ -> Error `Constant_expression_required )
            init
        in
        let value =
          match elem.mode with
          | Elem_active _ | Elem_passive -> Array.of_list init_as_ref
          | Elem_declarative ->
            (* Declarative element have no runtime value *)
            [||]
        in
        let env = Link_env.Build.add_elem i { value } env in
        let+ inits =
          match elem.mode with
          | Elem_active (None, _) -> assert false
          | Elem_active (Some table, offset) ->
            let length = Int32.of_int @@ List.length init in
            let* offset = Eval_const.expr env offset in
            let* offset = get_i32 offset in
            ok @@ (active_elem_expr ~offset ~length ~table ~elem:i :: inits)
          | Elem_passive | Elem_declarative -> Ok inits
        in
        (env, inits, succ i) )
      (env, [], 0) elem
  in
  (env, List.rev inits)

let populate_exports env (exports : Binary.exports) : exports Result.t =
  let fill_exports get_env exports names =
    list_fold_left
      (fun (acc, names) ({ name; id; _ } : Binary.export) ->
        let value = get_env env id in
        if StringSet.mem name names then Error `Duplicate_export_name
        else Ok (StringMap.add name value acc, StringSet.add name names) )
      (StringMap.empty, names) exports
  in
  let fill_exports' get_env exports names =
    list_fold_left
      (fun (acc, names) ({ name; id; _ } : Binary.export) ->
        let* value = get_env env id in
        if StringSet.mem name names then Error `Duplicate_export_name
        else Ok (StringMap.add name value acc, StringSet.add name names) )
      (StringMap.empty, names) exports
  in
  let names = StringSet.empty in
  let* globals, names =
    fill_exports' Link_env.get_global exports.global names
  in
  let* memories, names = fill_exports' Link_env.get_memory exports.mem names in
  let* tables, names = fill_exports' Link_env.get_table exports.table names in
  let* functions, names = fill_exports Link_env.get_func exports.func names in
  Ok { globals; memories; tables; functions; defined_names = names }

let modul (ls : 'f state) ~name (modul : Binary.Module.t) =
  Log.info (fun m -> m "linking      ...");
  let* envs, (env, init_active_data, init_active_elem) =
    Env_id.with_fresh_id
      (fun env_id ->
        let env = Link_env.Build.empty in
        let* env = eval_functions ls env_id env modul.func in
        let* env = eval_globals ls env modul.global in
        let* env = eval_memories ls env modul.mem in
        let* env = eval_tables ls env modul.table in
        let* env, init_active_data = define_data env modul.data in
        let+ env, init_active_elem = define_elem env modul.elem in
        let finished_env = Link_env.freeze env_id env ls.collection in
        (finished_env, (finished_env, init_active_data, init_active_elem)) )
      ls.envs
  in
  let* by_id_exports = populate_exports env modul.exports in
  let by_id =
    match modul.id with
    | None -> ls.by_id
    | Some id -> StringMap.add id (by_id_exports, Link_env.id env) ls.by_id
  in
  let by_name =
    match name with
    | None -> ls.by_name
    | Some name -> StringMap.add name by_id_exports ls.by_name
  in
  let start =
    Option.map (fun start_id -> [ Call (Raw start_id) ]) modul.start
  in
  let start = Option.fold ~none:[] ~some:(fun s -> [ s ]) start in
  let to_run = (init_active_data @ init_active_elem) @ start in
  let to_run = List.map Annotated.dummy_deep to_run in
  let module_to_run = { id = modul.id; env; to_run } in
  Ok
    ( module_to_run
    , { by_id
      ; by_name
      ; last = Some (by_id_exports, Link_env.id env)
      ; collection = ls.collection
      ; envs
      } )

let extern_module' (ls : 'f state) ~name ~(func_typ : 'f -> func_type)
  (module_ : 'f extern_module) =
  let functions, collection =
    List.fold_left
      (fun (functions, collection) (name, func) ->
        let typ = func_typ func in
        let id, collection = Func_id.add func typ collection in
        ((name, Func_intf.Extern id) :: functions, collection) )
      ([], ls.collection) module_.functions
  in
  let functions = StringMap.of_seq (List.to_seq functions) in
  let defined_names =
    StringMap.fold
      (fun name _ set -> StringSet.add name set)
      functions StringSet.empty
  in
  let exports =
    { functions
    ; globals = StringMap.empty
    ; memories = StringMap.empty
    ; tables = StringMap.empty
    ; defined_names
    }
  in
  { ls with by_name = StringMap.add name exports ls.by_name; collection }

let extern_module ls ~name modul =
  extern_module' ls ~name ~func_typ:Concrete_extern_func.extern_type modul

let register_module (ls : 'f state) ~name ~(id : string option) :
  'f state Result.t =
  let* exports, _env_id =
    match id with
    | Some id -> begin
      match StringMap.find_opt id ls.by_id with
      | None -> Error (`Unbound_module id)
      | Some e -> Ok e
    end
    | None -> (
      match ls.last with Some e -> Ok e | None -> Error `Unbound_last_module )
  in
  Ok { ls with by_name = StringMap.add name exports ls.by_name }

type extern_func = Concrete_extern_func.extern_func Func_id.collection
