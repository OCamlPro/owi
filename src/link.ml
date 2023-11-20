(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types
open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type global = Concrete_global.t

type table = Concrete_table.t

type func = Concrete_value.Func.t

type exports =
  { globals : global StringMap.t
  ; memories : Concrete_memory.t StringMap.t
  ; tables : table StringMap.t
  ; functions : func StringMap.t
  ; defined_names : StringSet.t
  }

type 'f module_to_run =
  { modul : Simplified.modul
  ; env : 'f Link_env.t
  ; to_run : simplified expr list
  }

type 'f envs = 'f Link_env.t Env_id.collection

type fenvs = Concrete_value.Func.extern_func Link_env.t Env_id.collection

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
  match StringMap.find import.modul ls.by_name with
  | exception Not_found -> error_s "unknown module %s" import.modul
  | exports -> (
    match StringMap.find import.name (f exports) with
    | exception Not_found ->
      Log.debug "unknown import %s" import.name;
      if StringSet.mem import.name exports.defined_names then
        Error "incompatible import type (Link.load_from_module)"
      else Error "unknown import"
    | v -> Ok v )

let load_global (ls : 'f state) (import : simplified global_type Imported.t) :
  global Result.t =
  let* global = load_from_module ls (fun (e : exports) -> e.globals) import in
  let* () =
    match (fst import.desc, global.mut) with
    | Var, Const | Const, Var ->
      Error "incompatible import type (Link.load_global)"
    | Const, Const | Var, Var -> Ok ()
  in
  if snd import.desc <> global.typ then begin
    Error "incompatible import type (Link.load_global bis)"
  end
  else Ok global

let eval_global ls env
  (global : (Simplified.global, simplified global_type) Runtime.t) :
  global Result.t =
  match global with
  | Local global ->
    let* value = Const_interp.exec_expr env global.init in
    let mut, typ = global.typ in
    let global : global = { value; label = global.id; mut; typ } in
    Ok global
  | Imported import -> load_global ls import

let eval_globals ls env globals : Link_env.Build.t Result.t =
  Named.fold
    (fun id global env ->
      let* env in
      let* global = eval_global ls env global in
      let env = Link_env.Build.add_global id global env in
      Ok env )
    globals (Ok env)

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
  else
    error_s "incompatible import type for memory %s %s expected %a got %a"
      import.modul import.name Pp.limits import.desc Pp.limits imported_limit

let eval_memory ls (memory : (mem, limits) Runtime.t) :
  Concrete_memory.t Result.t =
  match memory with
  | Local (_label, mem_type) -> ok @@ Concrete_memory.init mem_type
  | Imported import -> load_memory ls import

let eval_memories ls env memories =
  Named.fold
    (fun id mem env ->
      let* env in
      let* memory = eval_memory ls mem in
      let env = Link_env.Build.add_memory id memory env in
      Ok env )
    memories (Ok env)

let table_types_are_compatible (import, (t1 : simplified ref_type))
  (imported, t2) =
  limit_is_included ~import ~imported && t1 = t2

let load_table (ls : 'f state) (import : simplified table_type Imported.t) :
  table Result.t =
  let typ : simplified table_type = import.desc in
  let* t = load_from_module ls (fun (e : exports) -> e.tables) import in
  if table_types_are_compatible typ (t.limits, t.typ) then Ok t
  else
    error_s "incompatible import type for table %s %s expected %a got %a"
      import.modul import.name Pp.table_type typ Pp.table_type (t.limits, t.typ)

let eval_table ls (table : (_, simplified table_type) Runtime.t) :
  table Result.t =
  match table with
  | Local (label, table_type) -> ok @@ Concrete_table.init ?label table_type
  | Imported import -> load_table ls import

let eval_tables ls env tables =
  Named.fold
    (fun id table env ->
      let* env in
      let* table = eval_table ls table in
      let env = Link_env.Build.add_table id table env in
      Ok env )
    tables (Ok env)

let func_types_are_compatible a b =
  (* TODO: copied from Simplify_bis.equal_func_types => should factorize *)
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

let load_func (ls : 'f state)
  (import : (simplified, simplified) block_type Imported.t) : func Result.t =
  let (Bt_raw ((None | Some _), typ)) = import.desc in
  let* func = load_from_module ls (fun (e : exports) -> e.functions) import in
  let type' =
    match func with
    | Func_intf.WASM (_, func, _) ->
      let (Bt_raw ((None | Some _), t)) = func.type_f in
      t
    | Extern func_id -> Func_id.get_typ func_id ls.collection
  in
  if func_types_are_compatible typ type' then Ok func
  else Error "incompatible import type (Link.load_func)"

let eval_func ls (finished_env : Link_env.t') func : func Result.t =
  match func with
  | Runtime.Local func -> ok @@ Concrete_value.Func.wasm func finished_env
  | Imported import -> load_func ls import

let eval_functions ls (finished_env : Link_env.t') env functions =
  Named.fold
    (fun id func env ->
      let* env in
      let* func = eval_func ls finished_env func in
      let env = Link_env.Build.add_func id func env in
      Ok env )
    functions (Ok env)

let active_elem_expr ~offset ~length ~table ~elem =
  [ I32_const offset
  ; I32_const 0l
  ; I32_const length
  ; Table_init (Raw table, Raw elem)
  ; Elem_drop (Raw elem)
  ]

let active_data_expr ~offset ~length ~mem ~data =
  if mem <> 0 then begin
    error_s "wrong memory id: %i@." mem
  end
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
  | _ -> Error "type mismatch"

let define_data env data =
  Named.fold
    (fun id (data : Simplified.data) env_and_init ->
      let* env, init = env_and_init in
      let data' : Link_env.data = { value = data.init } in
      let env = Link_env.Build.add_data id data' env in
      let* init =
        match data.mode with
        | Data_active (None, _) -> assert false
        | Data_active (Some mem, offset) ->
          let* offset = Const_interp.exec_expr env offset in
          let length = Int32.of_int @@ String.length data.init in
          let* offset = get_i32 offset in
          let id = Raw id in
          let* v = active_data_expr ~offset ~length ~mem ~data:id in
          ok @@ (v :: init)
        | Data_passive -> Ok init
      in
      Ok (env, init) )
    data
    (Ok (env, []))

let define_elem env elem =
  Named.fold
    (fun id (elem : Simplified.elem) env_and_inits ->
      let* env, inits = env_and_inits in
      let* init = list_map (Const_interp.exec_expr env) elem.init in
      let* init_as_ref =
        list_map
          (fun v ->
            match v with
            | Concrete_value.Ref v -> Ok v
            | _ -> Error "constant expression required" )
          init
      in
      let env =
        match elem.mode with
        | Elem_active _ | Elem_passive ->
          Link_env.Build.add_elem id { value = Array.of_list init_as_ref } env
        | Elem_declarative ->
          (* Declarative element have no runtime value *)
          Link_env.Build.add_elem id { value = [||] } env
      in
      let* inits =
        match elem.mode with
        | Elem_active (None, _) -> assert false
        | Elem_active (Some table, offset) ->
          let length = Int32.of_int @@ List.length init in
          let* offset = Const_interp.exec_expr env offset in
          let* offset = get_i32 offset in
          ok @@ (active_elem_expr ~offset ~length ~table ~elem:id :: inits)
        | Elem_passive | Elem_declarative -> Ok inits
      in
      Ok (env, inits) )
    elem
    (Ok (env, []))

let populate_exports env (exports : Simplified.exports) : exports Result.t =
  let fill_exports get_env exports names =
    list_fold_left
      (fun (acc, names) (export : Simplified.export) ->
        let value = get_env env export.id in
        if StringSet.mem export.name names then Error "duplicate export name"
        else
          Ok
            ( StringMap.add export.name value acc
            , StringSet.add export.name names ) )
      (StringMap.empty, names) exports
  in
  let names = StringSet.empty in
  let* globals, names = fill_exports Link_env.get_global exports.global names in
  let* memories, names = fill_exports Link_env.get_memory exports.mem names in
  let* tables, names = fill_exports Link_env.get_table exports.table names in
  let* functions, names = fill_exports Link_env.get_func exports.func names in
  Ok { globals; memories; tables; functions; defined_names = names }

let modul (ls : 'f state) ~name (modul : Simplified.modul) =
  Log.debug "linking      ...@\n";
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
  let module_to_run = { modul; env; to_run } in
  Ok
    ( module_to_run
    , { by_id
      ; by_name
      ; last = Some (by_id_exports, Link_env.id env)
      ; collection = ls.collection
      ; envs
      } )

let extern_module' (ls : 'f state) ~name
  ~(func_typ : 'f -> simplified func_type) (module_ : 'f extern_module) =
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
  extern_module' ls ~name ~func_typ:Concrete_value.Func.extern_type modul

let register_module (ls : 'f state) ~name ~(id : string option) :
  'f state Result.t =
  let* exports, _env_id =
    match id with
    | Some id -> begin
      match StringMap.find_opt id ls.by_id with
      | None -> error_s "Unbound module id %s" id
      | Some e -> Ok e
    end
    | None -> (
      match ls.last with
      | Some e -> Ok e
      | None -> Error "No previous module to register" )
  in
  Ok { ls with by_name = StringMap.add name exports ls.by_name }

type extern_func = Concrete_value.Func.extern_func Func_id.collection
