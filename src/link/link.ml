(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* Link Linked_module *)

module Linked_module = struct
  module IMap = Map.Make (Int)

  type 'ext t =
    { globals : Concrete_global.t IMap.t
    ; memories : Concrete_memory.t IMap.t
    ; tables : Concrete_table.t IMap.t
    ; functions : Kind.func IMap.t
    ; data : Concrete_data.t IMap.t
    ; elem : Concrete_elem.t IMap.t
    ; tags : Binary.Tag.t IMap.t
    ; extern_funcs : ('ext * Binary.func_type) Dynarray.t
    ; id : int
    ; to_run : Binary.expr Annotated.t list
    }

  let get_id (modul : _ t) = modul.id

  let get_global (modul : _ t) id =
    match IMap.find_opt id modul.globals with
    | None -> assert false
    | Some v -> Concrete_choice.return v

  let get_memory (modul : _ t) id =
    match IMap.find_opt id modul.memories with
    | None -> assert false
    | Some v -> Concrete_choice.return v

  let get_table (modul : _ t) id =
    match IMap.find_opt id modul.tables with
    | None -> assert false
    | Some v -> Concrete_choice.return v

  let get_func (modul : _ t) id =
    match IMap.find_opt id modul.functions with
    | None -> assert false
    | Some v -> v

  let get_data (modul : _ t) id =
    match IMap.find_opt id modul.data with
    | None -> assert false
    | Some v -> Concrete_choice.return v

  let get_elem (modul : _ t) id =
    match IMap.find_opt id modul.elem with None -> assert false | Some v -> v

  let get_tag (modul : _ t) id =
    match IMap.find_opt id modul.tags with None -> assert false | Some v -> v

  let get_extern_func modul id =
    let f, _t = Dynarray.get modul.extern_funcs id in
    f

  let fold_globals f acc (modul : _ t) =
    IMap.fold (fun k v acc -> f k v acc) modul.globals acc

  module Build = struct
    type t =
      { globals : Concrete_global.t IMap.t
      ; memories : Concrete_memory.t IMap.t
      ; tables : Concrete_table.t IMap.t
      ; functions : Kind.func IMap.t
      ; data : Concrete_data.t IMap.t
      ; elem : Concrete_elem.t IMap.t
      ; tags : Binary.Tag.t IMap.t
      }

    let empty =
      { globals = IMap.empty
      ; memories = IMap.empty
      ; tables = IMap.empty
      ; functions = IMap.empty
      ; data = IMap.empty
      ; elem = IMap.empty
      ; tags = IMap.empty
      }

    let add_global id const (modul : t) =
      { modul with globals = IMap.add id const modul.globals }

    let add_memory id mem (modul : t) =
      { modul with memories = IMap.add id mem modul.memories }

    let add_table id table (modul : t) =
      { modul with tables = IMap.add id table modul.tables }

    let add_func id func (modul : t) =
      { modul with functions = IMap.add id func modul.functions }

    let add_data id data (modul : t) =
      { modul with data = IMap.add id data modul.data }

    let add_elem id elem (modul : t) =
      { modul with elem = IMap.add id elem modul.elem }

    let add_tag id tag (modul : t) =
      { modul with tags = IMap.add id tag modul.tags }

    let get_global (modul : t) id =
      match IMap.find_opt id modul.globals with
      | None -> Error (`Unknown_global (Text.Raw id))
      | Some v -> Ok v

    let get_const_global (modul : t) id =
      let* g = get_global modul id in
      match g.mut with
      | Const -> Result.ok g.value
      | Var -> Error `Constant_expression_required

    let get_func (modul : t) id =
      match IMap.find_opt id modul.functions with
      | None -> Error (`Unknown_func (Text.Raw id))
      | Some v -> Ok v

    let get_memories { memories; _ } = memories
  end

  let freeze id
    ({ globals; memories; tables; functions; data; elem; tags } : Build.t)
    to_run extern_funcs =
    { id
    ; globals
    ; memories
    ; tables
    ; functions
    ; data
    ; elem
    ; tags
    ; extern_funcs
    ; to_run
    }

  let get_expr_to_run { to_run; _ } = to_run
end

(* Link State *)
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
    ; tags : Binary.Tag.t StringMap.t
    ; defined_names : StringSet.t
    }

  type 'f modules = 'f Linked_module.t Dynarray.t

  type 'f t =
    { by_name : exports StringMap.t
    ; by_id : (exports * int) StringMap.t
    ; last : (exports * int) option
    ; collection : ('f * Binary.func_type) Dynarray.t
    ; modules : 'f modules
    }

  let empty () =
    { by_name = StringMap.empty
    ; by_id = StringMap.empty
    ; last = None
    ; collection = Dynarray.create ()
    ; modules = Dynarray.create ()
    }

  (* TODO: I'm not sure it makes sense to try making the Link.State.t persistent, we could change the API to be fully mutable? *)
  let clone { by_name; by_id; last; collection; modules } =
    let collection = Dynarray.copy collection in
    let modules = Dynarray.copy modules in
    { by_name; by_id; last; collection; modules }

  let get_modules state = state.modules

  let get_last state = state.last

  let get_by_id state id = StringMap.find_opt id state.by_id

  let get_module ls mod_id =
    match mod_id with
    | None ->
      begin match get_last ls with
      | None -> Error `Unbound_last_module
      | Some m -> Ok m
      end
    | Some mod_id -> (
      match get_by_id ls mod_id with
      | None -> Error (`Unbound_module mod_id)
      | Some exports -> Ok exports )

  let get_global_from_module state mod_id global_name =
    let* exports, _module_id = get_module state mod_id in
    match StringMap.find_opt global_name exports.globals with
    | None -> Error (`Unbound_name global_name)
    | Some v -> Ok v

  let get_func_from_module state mod_id func_name =
    let* exports, modul_id = get_module state mod_id in
    match StringMap.find_opt func_name exports.functions with
    | None -> Error (`Unbound_name func_name)
    | Some v -> Ok (v, modul_id)

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

  let load_global (ls : 'f t) (import : Binary.Global.Type.t Origin.imported) :
    global Result.t =
    let* global = load_from_module ls (fun (e : exports) -> e.globals) import in
    let* strict =
      match (fst import.typ, global.mut) with
      | Var, Const | Const, Var -> Error (`Incompatible_import_type import.name)
      | Const, Const -> Ok false
      | Var, Var -> Ok true
    in
    if
      not
        ( if strict then Binary.val_type_eq global.typ (snd import.typ)
          else Binary.is_subtype_val_type global.typ (snd import.typ) )
    then begin
      Error (`Incompatible_import_type import.name)
    end
    else Ok global

  let register_last_module (ls : 'f t) ~name ~(id : string option) :
    'f t Result.t =
    let* exports, _modul_id =
      match id with
      | Some id ->
        begin match StringMap.find_opt id ls.by_id with
        | None -> Error (`Unbound_module id)
        | Some e -> Ok e
        end
      | None -> (
        match ls.last with Some e -> Ok e | None -> Error `Unbound_last_module )
    in
    Ok { ls with by_name = StringMap.add name exports ls.by_name }
end

(* TODO; the const evaluation is duplicated in many places and should be moved somewhere else! *)
module Eval_const = struct
  module Stack = Stack.Make [@inlined hint] (Concrete_value)

  let i32_instr stack : Binary.i32_instr -> _ = function
    | Const i -> Stack.push_i32 stack i
    | Add -> Stack.apply_i32_i32_i32 stack Concrete_i32.add
    | Sub -> Stack.apply_i32_i32_i32 stack Concrete_i32.sub
    | Mul -> Stack.apply_i32_i32_i32 stack Concrete_i32.mul
    | _ -> assert false

  let i64_instr stack : Binary.i64_instr -> _ = function
    | Const i -> Stack.push_i64 stack i
    | Add -> Stack.apply_i64_i64_i64 stack Concrete_i64.add
    | Sub -> Stack.apply_i64_i64_i64 stack Concrete_i64.sub
    | Mul -> Stack.apply_i64_i64_i64 stack Concrete_i64.mul
    | _ -> assert false

  let instr modul stack instr =
    match instr.Annotated.raw with
    | Binary.I32 i -> Result.ok (i32_instr stack i)
    | Binary.I64 i -> Result.ok (i64_instr stack i)
    | F32 (Const f) -> Result.ok @@ Stack.push_f32 stack f
    | F64 (Const f) -> Result.ok @@ Stack.push_f64 stack f
    | V128 (Const f) -> Result.ok @@ Stack.push_v128 stack f
    | Ref (Null t) -> Result.ok @@ Stack.push_ref stack (Concrete_ref.null t)
    | Ref (Func f) ->
      let* f = Linked_module.Build.get_func modul f in
      let value = Concrete_value.Ref (Func (Some f)) in
      Result.ok @@ Stack.push stack value
    | Global (Get id) ->
      let* g = Linked_module.Build.get_const_global modul id in
      Result.ok @@ Stack.push stack g
    | _ -> assert false

  (* TODO: binary+const expr *)
  let expr modul e : Concrete_value.t Result.t =
    let* stack = list_fold_left (instr modul) Stack.empty e.Annotated.raw in
    match stack with
    | [] -> Error (`Type_mismatch "const expr returning zero values")
    | _ :: _ :: _ ->
      Error (`Type_mismatch "const expr returning more than one value")
    | [ result ] -> Ok result
end

let eval_global ls modul
  (global : (Binary.Global.t, Binary.Global.Type.t) Origin.t) : global Result.t
    =
  match global with
  | Local global ->
    let* value = Eval_const.expr modul global.init in
    let mut, typ = global.typ in
    let global : global = { value; mut; typ } in
    Ok global
  | Imported import -> State.load_global ls import

let eval_globals ls modul globals : Linked_module.Build.t Result.t =
  let+ modul, _i =
    array_fold_left
      (fun (modul, i) global ->
        let+ global = eval_global ls modul global in
        let modul = Linked_module.Build.add_global i global modul in
        (modul, succ i) )
      (modul, 0) globals
  in
  modul

let memory_limit_is_included ~import ?imported_data_size ~imported () =
  match (import, imported) with
  | Binary.Mem.Type.I32 import, Binary.Mem.Type.I32 imported ->
    Int32.(
      le_u import.min
        (Option.fold ~none:imported.min
           ~some:(fun v -> of_int v)
           imported_data_size ) )
    && begin match (imported.max, import.max) with
    | _, None -> true
    | None, Some _ -> false
    | Some i, Some j -> Int32.le_u i j
    end
  | I64 import, I64 imported ->
    import.min
    <= Option.fold ~none:imported.min ~some:(fun v -> v) imported_data_size
    && begin match (imported.max, import.max) with
    | _, None -> true
    | None, Some _ -> false
    | Some i, Some j -> i <= j
    end
  | _ -> false

let table_limit_is_included ~import ?imported_data_size ~imported () =
  match (import, imported) with
  | Binary.Table.Type.I32 import, Binary.Table.Type.I32 imported ->
    Int32.(
      le_u import.min
        (Option.fold ~none:imported.min
           ~some:(fun v -> of_int v)
           imported_data_size ) )
    && begin match (imported.max, import.max) with
    | _, None -> true
    | None, Some _ -> false
    | Some i, Some j -> Int32.le_u i j
    end
  | I64 import, I64 imported ->
    Int64.(
      le_u import.min
        (Option.fold ~none:imported.min
           ~some:(fun v -> of_int v)
           imported_data_size ) )
    && begin match (imported.max, import.max) with
    | _, None -> true
    | None, Some _ -> false
    | Some i, Some j -> Int64.le_u i j
    end
  | _ -> false

let load_memory (ls : 'f State.t)
  (import : Binary.Mem.Type.limits Origin.imported) : Concrete_memory.t Result.t
    =
  let* mem =
    State.load_from_module ls (fun (e : State.exports) -> e.memories) import
  in
  let imported_limit = Concrete_memory.get_limits mem in
  if memory_limit_is_included ~import:import.typ ~imported:imported_limit ()
  then Ok mem
  else Error (`Incompatible_import_type import.name)

let eval_memory ls (memory : (Binary.Mem.t, Binary.Mem.Type.limits) Origin.t) :
  Concrete_memory.t Result.t =
  match memory with
  | Local (_label, mem_type) -> Result.ok @@ Concrete_memory.init mem_type
  | Imported import -> load_memory ls import

let eval_memories ls modul memories =
  let+ modul, _i =
    array_fold_left
      (fun (modul, id) mem ->
        let+ memory = eval_memory ls mem in
        let modul = Linked_module.Build.add_memory id memory modul in
        (modul, succ id) )
      (modul, 0) memories
  in
  modul

let table_types_are_compatible ~imported_data_size
  (import, (t1 : Binary.ref_type)) (imported, t2) =
  table_limit_is_included ~imported_data_size ~import ~imported ()
  && Binary.ref_type_eq t1 t2

let load_table (ls : 'f State.t) (import : Binary.Table.Type.t Origin.imported)
  : table Result.t =
  let typ : Binary.Table.Type.t = import.typ in
  let* t =
    State.load_from_module ls (fun (e : State.exports) -> e.tables) import
  in
  let imported_data_size = Concrete_table.size t in
  if table_types_are_compatible typ ~imported_data_size (t.limits, t.typ) then
    Ok t
  else Error (`Incompatible_import_type import.name)

let eval_table ls (table : (Binary.Table.t, Binary.Table.Type.t) Origin.t) :
  table Result.t =
  match table with
  | Local { id = label; typ; _ } -> Result.ok @@ Concrete_table.init ?label typ
  | Imported import -> load_table ls import

let eval_tables ls modul tables =
  let+ modul, _i =
    array_fold_left
      (fun (modul, i) table ->
        let+ table = eval_table ls table in
        let modul = Linked_module.Build.add_table i table modul in
        (modul, succ i) )
      (modul, 0) tables
  in
  modul

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
  if Binary.func_type_eq typ type' then Ok func
  else
    let msg =
      Fmt.str "%s: expected: %a got: %a" import.name Binary.pp_func_type typ
        Binary.pp_func_type type'
    in
    Error (`Incompatible_import_type msg)

let eval_func ls (finished_modul : int) func : func Result.t =
  match func with
  | Origin.Local func -> Result.ok @@ Kind.wasm func finished_modul
  | Imported import -> load_func ls import

let eval_functions ls (finished_modul : int) modul functions =
  let+ modul, _i =
    array_fold_left
      (fun (modul, i) func ->
        let+ func = eval_func ls finished_modul func in
        let modul = Linked_module.Build.add_func i func modul in
        (modul, succ i) )
      (modul, 0) functions
  in
  modul

let eval_tag ls (_finished_modul : int)
  (tag : (Binary.Tag.t, Binary.block_type) Origin.t) : Binary.Tag.t Result.t =
  match tag with
  | Origin.Local tag -> Ok tag
  | Imported import ->
    let (Binary.Bt_raw ((None | Some _), import_typ)) = import.typ in
    let* tag =
      State.load_from_module ls (fun (e : State.exports) -> e.tags) import
    in
    let (Bt_raw ((None | Some _), typ)) = tag.typ in
    if Binary.func_type_eq typ import_typ then Ok tag
    else
      let msg =
        Fmt.str "%s: expected: %a got: %a" import.name Binary.pp_func_type
          import_typ Binary.pp_func_type typ
      in
      Error (`Incompatible_import_type msg)

let eval_tags ls (finished_modul : int) modul
  (tags : (Binary.Tag.t, Binary.block_type) Origin.t array) =
  let+ modul, _i =
    array_fold_left
      (fun (modul, i) tag ->
        let+ tag = eval_tag ls finished_modul tag in
        let modul = Linked_module.Build.add_tag i tag modul in
        (modul, succ i) )
      (modul, 0) tags
  in
  modul

let active_elem_expr ~offset ~length ~table ~elem =
  [ Binary.I32 (Const offset)
  ; I32 (Const 0l)
  ; I32 (Const length)
  ; Table (Init (table, elem))
  ; Elem (Drop elem)
  ]

let active_data_expr modul ~offset ~length ~mem ~data =
  if not (Linked_module.IMap.mem mem (Linked_module.Build.get_memories modul))
  then Error (`Unknown_memory (Text.Raw mem))
  else
    Ok
      [ Binary.I32 (Const offset)
      ; I32 (Const 0l)
      ; I32 (Const length)
      ; Memory (Init (mem, data))
      ; Data (Drop data)
      ]

let get_i32 = function
  | Concrete_value.I32 i -> Ok i
  | _ -> Error (`Type_mismatch "get_i32")

let define_data modul data =
  let+ modul, init, _i =
    array_fold_left
      (fun (modul, init, id) (data : Binary.Data.t) ->
        let data' : Concrete_data.t = { value = data.init } in
        let modul = Linked_module.Build.add_data id data' modul in
        let+ init =
          match data.mode with
          | Active (mem, offset) ->
            let* offset = Eval_const.expr modul offset in
            let length = Int32.of_int @@ String.length data.init in
            let* offset = get_i32 offset in
            let* v = active_data_expr modul ~offset ~length ~mem ~data:id in
            Result.ok @@ (v :: init)
          | Passive -> Ok init
        in
        (modul, init, succ id) )
      (modul, [], 0) data
  in
  (modul, List.rev init)

let define_elem modul elem =
  let+ modul, inits, _i =
    array_fold_left
      (fun (modul, inits, i) (elem : Binary.Elem.t) ->
        let* init = list_map (Eval_const.expr modul) elem.init in
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
        let modul = Linked_module.Build.add_elem i { value } modul in
        let+ inits =
          match elem.mode with
          | Active (None, _) -> assert false
          | Active (Some table, offset) ->
            let length = Int32.of_int @@ List.length init in
            let* offset = Eval_const.expr modul offset in
            let* offset = get_i32 offset in
            Result.ok
            @@ (active_elem_expr ~offset ~length ~table ~elem:i :: inits)
          | Passive | Declarative -> Ok inits
        in
        (modul, inits, succ i) )
      (modul, [], 0) elem
  in
  (modul, List.rev inits)

let populate_exports modul (exports : Binary.Module.Exports.t) :
  State.exports Result.t =
  let fill_exports get_modul exports names =
    array_fold_left
      (fun (acc, names) ({ name; id; _ } : Binary.Export.t) ->
        let value = get_modul modul id in
        if StringSet.mem name names then Error `Duplicate_export_name
        else Ok (StringMap.add name value acc, StringSet.add name names) )
      (StringMap.empty, names) exports
  in
  let fill_exports' get_modul exports names =
    array_fold_left
      (fun (acc, names) ({ name; id; _ } : Binary.Export.t) ->
        let* value = get_modul modul id in
        if StringSet.mem name names then Error `Duplicate_export_name
        else Ok (StringMap.add name value acc, StringSet.add name names) )
      (StringMap.empty, names) exports
  in
  let names = StringSet.empty in
  let* globals, names =
    fill_exports' Linked_module.get_global exports.global names
  in
  let* memories, names =
    fill_exports' Linked_module.get_memory exports.mem names
  in
  let* tables, names =
    fill_exports' Linked_module.get_table exports.table names
  in
  let* functions, names =
    fill_exports Linked_module.get_func exports.func names
  in
  let+ tags, names = fill_exports Linked_module.get_tag exports.tag names in
  { State.globals; memories; tables; functions; tags; defined_names = names }

module Binary = struct
  let aux ~name (ls : 'f State.t) (binary_module : Binary.Module.t) =
    Log.info (fun m -> m "linking      ...");
    let ls = State.clone ls in
    let next_id = Dynarray.length ls.modules in
    let modul = Linked_module.Build.empty in
    let* modul = eval_functions ls next_id modul binary_module.func in
    let* modul = eval_tags ls next_id modul binary_module.tag in
    let* modul = eval_globals ls modul binary_module.global in
    let* modul = eval_memories ls modul binary_module.mem in
    let* modul = eval_tables ls modul binary_module.table in
    let* modul, init_active_data = define_data modul binary_module.data in
    let* modul, init_active_elem = define_elem modul binary_module.elem in
    let to_run =
      let start =
        Option.map
          (fun start_id -> [ Binary.Call start_id ])
          binary_module.start
        |> Option.fold ~none:[] ~some:(fun s -> [ s ])
      in
      (init_active_elem @ init_active_data) @ start
      |> List.map Annotated.dummy_deep
    in

    let modul : _ Linked_module.t =
      Linked_module.freeze next_id modul to_run ls.collection
    in
    Dynarray.add_last ls.modules modul;

    let+ by_id_exports = populate_exports modul binary_module.exports in
    let by_id =
      match binary_module.id with
      | None -> ls.by_id
      | Some id ->
        StringMap.add id (by_id_exports, Linked_module.get_id modul) ls.by_id
    in
    let by_name =
      match name with
      | None -> ls.by_name
      | Some name -> StringMap.add name by_id_exports ls.by_name
    in

    ( modul
    , { State.by_id
      ; by_name
      ; last = Some (by_id_exports, Linked_module.get_id modul)
      ; collection = ls.collection
      ; modules = ls.modules
      } )

  let concrete_module ~name link_state modul = aux ~name link_state modul

  let symbolic_module ~name link_state modul = aux ~name link_state modul

  let abstract_module ~name link_state modul = aux ~name link_state modul
end

module Extern = struct
  let aux ~name (ls : 'f State.t) func_type functions =
    let functions, collection =
      List.fold_left
        (fun (functions, collection) (name, func) ->
          let typ = func_type func in
          Dynarray.add_last collection (func, typ);
          let id = Dynarray.length collection - 1 in
          ((name, (Kind.extern id : Kind.func)) :: functions, collection) )
        ([], ls.collection) functions
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
      ; tags = StringMap.empty
      ; defined_names
      }
    in
    { ls with by_name = StringMap.add name exports ls.by_name; collection }

  let concrete_module ~name (modul : Concrete_extern.Module.t)
    (link_state : Concrete_extern.Func.t State.t) =
    aux ~name link_state Concrete_extern.Func.to_func_type modul

  let symbolic_module ~name (modul : Symbolic_extern.Module.t)
    (link_state : Symbolic_extern.Func.t State.t) =
    aux ~name link_state Symbolic_extern.Func.to_func_type modul

  let abstract_module ~name (modul : Abstract_extern.Module.t)
    (link_state : Abstract_extern.Func.t State.t) =
    aux ~name link_state Abstract_extern.Func.to_func_type modul
end
