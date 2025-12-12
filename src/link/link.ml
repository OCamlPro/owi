(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type global = Concrete_global.t

type table = Concrete_table.t

type func = Kind.func

module State = struct
  type exports =
    { globals : global StringMap.t
    ; memories : Concrete_memory.t StringMap.t
    ; tables : table StringMap.t
    ; functions : func StringMap.t
    ; defined_names : StringSet.t
    }

  type 'f envs = 'f Link_env.t Dynarray.t

  type 'f t =
    { by_name : exports StringMap.t
    ; by_id : (exports * int) StringMap.t
    ; last : (exports * int) option
    ; collection : ('f * Text.func_type) Dynarray.t
    ; envs : 'f envs
    }

  let empty () =
    { by_name = StringMap.empty
    ; by_id = StringMap.empty
    ; last = None
    ; collection = Dynarray.create ()
    ; envs = Dynarray.create ()
    }

  (* TODO: I'm not sure it makes sense to try making the Link.State.t persistent, we could change the API to be fully mutable? *)
  let clone { by_name; by_id; last; collection; envs } =
    let collection = Dynarray.copy collection in
    let envs = Dynarray.copy envs in
    { by_name; by_id; last; collection; envs }

  let get_envs state = state.envs

  let get_last state = state.last

  let get_by_id state id = StringMap.find_opt id state.by_id

  let load_from_module ls f (import : _ Origin.imported) =
    match StringMap.find_opt import.modul_name ls.by_name with
    | None -> Error (`Unknown_module import.modul_name)
    | Some exports -> (
      match StringMap.find_opt import.name (f exports) with
      | None ->
        if StringSet.mem import.name exports.defined_names then
          Error (`Incompatible_import_type import.name)
        else Error (`Unknown_import (import.modul_name, import.name))
      | Some v -> Ok v )

  let load_global (ls : 'f t) (import : Text.Global.Type.t Origin.imported) :
    global Result.t =
    let* global = load_from_module ls (fun (e : exports) -> e.globals) import in
    let* () =
      match (fst import.typ, global.mut) with
      | Var, Const | Const, Var -> Error (`Incompatible_import_type import.name)
      | Const, Const | Var, Var -> Ok ()
    in
    if not @@ Text.val_type_eq (snd import.typ) global.typ then begin
      Error (`Incompatible_import_type import.name)
    end
    else Ok global
end

(* TODO; the const evaluation is duplicated in many places and should be moved somewhere else! *)
module Eval_const = struct
  module Stack = Stack.Make [@inlined hint] (Concrete_value)

  (* TODO: const ibinop *)
  let ibinop stack nn (op : Text.ibinop) =
    match nn with
    | Text.S32 ->
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
    | Binary.I32_const n -> ok @@ Stack.push_i32 stack n
    | I64_const n -> ok @@ Stack.push_i64 stack n
    | F32_const f -> ok @@ Stack.push_f32 stack f
    | F64_const f -> ok @@ Stack.push_f64 stack f
    | V128_const f -> ok @@ Stack.push_v128 stack f
    | I_binop (nn, op) -> ok @@ ibinop stack nn op
    | Ref_null t -> ok @@ Stack.push_ref stack (Concrete_ref.null t)
    | Ref_func f ->
      let* f = Link_env.Build.get_func env f in
      let value = Concrete_value.Ref (Func (Some f)) in
      ok @@ Stack.push stack value
    | Global_get id ->
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

let eval_global ls env (global : (Binary.Global.t, Text.Global.Type.t) Origin.t)
  : global Result.t =
  match global with
  | Local global ->
    let* value = Eval_const.expr env global.init in
    let mut, typ = global.typ in
    let global : global = { value; mut; typ } in
    Ok global
  | Imported import -> State.load_global ls import

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

(* TODO: IIRC this is duplicated and should be refactored *)
let limit_is_included ~import ~imported =
  imported.Text.min >= import.Text.min
  &&
  match (imported.max, import.max) with
  | _, None -> true
  | None, Some _ -> false
  | Some i, Some j -> i <= j

let load_memory (ls : 'f State.t) (import : Text.limits Origin.imported) :
  Concrete_memory.t Result.t =
  let* mem =
    State.load_from_module ls (fun (e : State.exports) -> e.memories) import
  in
  let imported_limit = Concrete_memory.get_limits mem in
  if limit_is_included ~import:import.typ ~imported:imported_limit then Ok mem
  else Error (`Incompatible_import_type import.name)

let eval_memory ls (memory : (Text.Mem.t, Text.limits) Origin.t) :
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

let table_types_are_compatible (import, (t1 : Text.ref_type)) (imported, t2) =
  limit_is_included ~import ~imported && Text.ref_type_eq t1 t2

let load_table (ls : 'f State.t) (import : Text.Table.Type.t Origin.imported) :
  table Result.t =
  let typ : Text.Table.Type.t = import.typ in
  let* t =
    State.load_from_module ls (fun (e : State.exports) -> e.tables) import
  in
  if table_types_are_compatible typ (t.limits, t.typ) then Ok t
  else Error (`Incompatible_import_type import.name)

let eval_table ls (table : (_, Text.Table.Type.t) Origin.t) : table Result.t =
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

let load_func (ls : 'f State.t) (import : Binary.block_type Origin.imported) :
  func Result.t =
  let (Binary.Bt_raw ((None | Some _), typ)) = import.typ in
  let* func =
    State.load_from_module ls (fun (e : State.exports) -> e.functions) import
  in
  let type' =
    match func with
    | Kind.Wasm { func; _ } ->
      let (Bt_raw ((None | Some _), t)) = func.type_f in
      t
    | Extern { idx } ->
      let _f, t = Dynarray.get ls.collection idx in
      t
  in
  if Text.func_type_eq typ type' then Ok func
  else
    let msg =
      Fmt.str "%s: expected: %a got: %a" import.name Text.pp_func_type typ
        Text.pp_func_type type'
    in
    Error (`Incompatible_import_type msg)

let eval_func ls (finished_env : int) func : func Result.t =
  match func with
  | Origin.Local func -> ok @@ Kind.wasm func finished_env
  | Imported import -> load_func ls import

let eval_functions ls (finished_env : int) env functions =
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
  [ Binary.I32_const offset
  ; I32_const 0l
  ; I32_const length
  ; Table_init (table, elem)
  ; Elem_drop elem
  ]

let active_data_expr env ~offset ~length ~mem ~data =
  if not (Link_env.IMap.mem mem (Link_env.Build.get_memories env)) then
    Error (`Unknown_memory (Text.Raw mem))
  else
    Ok
      [ Binary.I32_const offset
      ; I32_const 0l
      ; I32_const length
      ; Memory_init (mem, data)
      ; Data_drop data
      ]

let get_i32 = function
  | Concrete_value.I32 i -> Ok i
  | _ -> Error (`Type_mismatch "get_i32")

let define_data env data =
  let+ env, init, _i =
    array_fold_left
      (fun (env, init, id) (data : Binary.Data.t) ->
        let data' : Concrete_data.t = { value = data.init } in
        let env = Link_env.Build.add_data id data' env in
        let+ init =
          match data.mode with
          | Active (mem, offset) ->
            let* offset = Eval_const.expr env offset in
            let length = Int32.of_int @@ String.length data.init in
            let* offset = get_i32 offset in
            let* v = active_data_expr env ~offset ~length ~mem ~data:id in
            ok @@ (v :: init)
          | Passive -> Ok init
        in
        (env, init, succ id) )
      (env, [], 0) data
  in
  (env, List.rev init)

let define_elem env elem =
  let+ env, inits, _i =
    array_fold_left
      (fun (env, inits, i) (elem : Binary.Elem.t) ->
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
          | Active _ | Passive -> Array.of_list init_as_ref
          | Declarative ->
            (* Declarative element have no runtime value *)
            [||]
        in
        let env = Link_env.Build.add_elem i { value } env in
        let+ inits =
          match elem.mode with
          | Active (None, _) -> assert false
          | Active (Some table, offset) ->
            let length = Int32.of_int @@ List.length init in
            let* offset = Eval_const.expr env offset in
            let* offset = get_i32 offset in
            ok @@ (active_elem_expr ~offset ~length ~table ~elem:i :: inits)
          | Passive | Declarative -> Ok inits
        in
        (env, inits, succ i) )
      (env, [], 0) elem
  in
  (env, List.rev inits)

let populate_exports env (exports : Binary.Module.Exports.t) :
  State.exports Result.t =
  let fill_exports get_env exports names =
    array_fold_left
      (fun (acc, names) ({ name; id; _ } : Binary.Export.t) ->
        let value = get_env env id in
        if StringSet.mem name names then Error `Duplicate_export_name
        else Ok (StringMap.add name value acc, StringSet.add name names) )
      (StringMap.empty, names) exports
  in
  let fill_exports' get_env exports names =
    array_fold_left
      (fun (acc, names) ({ name; id; _ } : Binary.Export.t) ->
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
  let+ functions, names = fill_exports Link_env.get_func exports.func names in
  { State.globals; memories; tables; functions; defined_names = names }

module Binary = struct
  let modul ~name (ls : 'f State.t) (modul : Binary.Module.t) =
    Log.info (fun m -> m "linking      ...");
    let ls = State.clone ls in
    let next_id = Dynarray.length ls.envs in
    let env = Link_env.Build.empty in
    let* env = eval_functions ls next_id env modul.func in
    let* env = eval_globals ls env modul.global in
    let* env = eval_memories ls env modul.mem in
    let* env = eval_tables ls env modul.table in
    let* env, init_active_data = define_data env modul.data in
    let* env, init_active_elem = define_elem env modul.elem in
    let env = Link_env.freeze next_id env ls.collection in
    Dynarray.add_last ls.envs env;
    let+ by_id_exports = populate_exports env modul.exports in
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
      Option.map (fun start_id -> [ Binary.Call start_id ]) modul.start
    in
    let start = Option.fold ~none:[] ~some:(fun s -> [ s ]) start in
    let to_run = (init_active_elem @ init_active_data) @ start in
    let to_run = List.map Annotated.dummy_deep to_run in
    let modul = { Linked.Module.id = modul.id; env; to_run } in
    ( modul
    , { State.by_id
      ; by_name
      ; last = Some (by_id_exports, Link_env.id env)
      ; collection = ls.collection
      ; envs = ls.envs
      } )
end

module Extern = struct
  let modul ~name (modul : 'f Extern.Module.t) (ls : 'f State.t) =
    let functions, collection =
      List.fold_left
        (fun (functions, collection) (name, func) ->
          let typ = modul.func_type func in
          Dynarray.add_last collection (func, typ);
          let id = Dynarray.length collection - 1 in
          ((name, (Kind.extern id : Kind.func)) :: functions, collection) )
        ([], ls.collection) modul.functions
    in
    let functions = StringMap.of_seq (List.to_seq functions) in
    let defined_names =
      StringMap.fold
        (fun name _ set -> StringSet.add name set)
        functions StringSet.empty
    in
    let exports =
      { State.functions
      ; globals = StringMap.empty
      ; memories = StringMap.empty
      ; tables = StringMap.empty
      ; defined_names
      }
    in
    { ls with by_name = StringMap.add name exports ls.by_name; collection }
end

let register_last_module (ls : 'f State.t) ~name ~(id : string option) :
  'f State.t Result.t =
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
