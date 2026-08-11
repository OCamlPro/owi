(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module IMap = Map.Make (Int)
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let get_unsafe id map =
  match IMap.find_opt id map with Some v -> v | None -> assert false

(* Link Linked_module *)
module Make (M : Link_intf.M) = struct
  type extern_module = M.extern_module

  module Linked_module = struct
    type t =
      { globals : Concrete_global.t IMap.t
      ; memories : Concrete_memory.t IMap.t
      ; tables : Concrete_table.t IMap.t
      ; functions : Kind.func IMap.t
      ; data : M.data IMap.t
      ; elem : Concrete_elem.t IMap.t
      ; tags : Binary.Tag.t IMap.t
      ; extern_funcs : (M.extern_func * Binary.func_type) Dynarray.t
      ; id : int
      ; init_code : Binary.expr Annotated.t
      ; types : Binary.sub_type array
      ; type_groups : (int * int) array
      }

    let get_id (modul : t) = modul.id

    let get_global (modul : t) id = get_unsafe id modul.globals

    let get_memory (modul : t) id = get_unsafe id modul.memories

    let get_table (modul : t) id = get_unsafe id modul.tables

    let get_func (modul : t) id = get_unsafe id modul.functions

    let get_data (modul : t) id = get_unsafe id modul.data

    let get_elem (modul : t) id = get_unsafe id modul.elem

    let get_tag (modul : t) id = get_unsafe id modul.tags

    let get_extern_func modul id =
      let f, _t = Dynarray.get modul.extern_funcs id in
      f

    let fold_globals f acc (modul : t) =
      IMap.fold (fun k v acc -> f k v acc) modul.globals acc

    module Build = struct
      type t =
        { globals : Concrete_global.t IMap.t
        ; memories : Concrete_memory.t IMap.t
        ; tables : Concrete_table.t IMap.t
        ; functions : Kind.func IMap.t
        ; data : M.data IMap.t
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
        let data = M.data_of_concrete data in
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
      init_code extern_funcs types type_defs =
      let type_groups =
        Binary.compute_type_groups type_defs (Array.length types)
      in
      { id
      ; globals
      ; memories
      ; tables
      ; functions
      ; data
      ; elem
      ; tags
      ; extern_funcs
      ; init_code
      ; types
      ; type_groups
      }

    let get_types { types; _ } = types

    let get_type_groups { type_groups; _ } = type_groups

    let get_init_code { init_code; _ } = init_code
  end

  (* Link State *)

  type global = Concrete_global.t

  type table = Concrete_table.t

  type func = Kind.func

  type exports =
    { globals : global StringMap.t
    ; memories : Concrete_memory.t StringMap.t
    ; tables : table StringMap.t
    ; functions : func StringMap.t
    ; tags : Binary.Tag.t StringMap.t
    ; defined_names : StringSet.t
    }

  type t =
    { by_name : exports StringMap.t
    ; by_id : (exports * int) StringMap.t
    ; last : (exports * int) option
    ; extern_modules : (M.extern_func * Binary.func_type) Dynarray.t
    ; modules : Linked_module.t Dynarray.t
    }

  let empty () =
    { by_name = StringMap.empty
    ; by_id = StringMap.empty
    ; last = None
    ; extern_modules = Dynarray.create ()
    ; modules = Dynarray.create ()
    }

  (* TODO: I'm not sure it makes sense to try making the Link.State.t persistent, we could change the API to be fully mutable? *)
  let clone { by_name; by_id; last; extern_modules; modules } =
    let extern_modules = Dynarray.copy extern_modules in
    let modules = Dynarray.copy modules in
    { by_name; by_id; last; extern_modules; modules }

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

  let get_exported_global state ~module_name ~global_name =
    let* exports, _module_id = get_module state module_name in
    match StringMap.find_opt global_name exports.globals with
    | None -> Error (`Unbound_name global_name)
    | Some v -> Ok v

  let get_exported_func state ~module_name ~func_name =
    let* exports, modul_id = get_module state module_name in
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

  let load_global (ls : t) (import : Binary.Global.Type.t Origin.imported) :
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

  let register_last_module (ls : t) ~name ~(id : string option) : t Result.t =
    let* exports, _modul_id =
      match id with
      | Some id ->
        begin match StringMap.find_opt id ls.by_id with
        | None -> Error (`Unbound_module id)
        | Some e -> Ok e
        end
      | None -> (
        match ls.last with Some e -> Ok e | None -> Error `Unbound_last_module )
      (* TODO; the const evaluation is duplicated in many places and should be moved somewhere else! *)
    in
    Ok { ls with by_name = StringMap.add name exports ls.by_name }

  let get_module (state : t) (i : int) = Dynarray.get state.modules i

  let get_memory ~modul state i =
    let modul = get_module state modul in
    Linked_module.get_memory modul i

  let get_data ~modul state i =
    let modul = get_module state modul in
    Linked_module.get_data modul i

  let get_func ~modul state i =
    let modul = get_module state modul in
    Linked_module.get_func modul i

  let get_table ~modul state i =
    let modul = get_module state modul in
    Linked_module.get_table modul i

  let get_elem ~modul state i =
    let modul = get_module state modul in
    Linked_module.get_elem modul i

  let get_global ~modul state i =
    let modul = get_module state modul in
    Linked_module.get_global modul i

  let get_extern_func ~modul state i =
    let modul = get_module state modul in
    Linked_module.get_extern_func modul i

  let get_init_code ~modul state =
    let modul = get_module state modul in
    Linked_module.get_init_code modul

  let get_types ~modul state =
    let modul = get_module state modul in
    Linked_module.get_types modul

  let get_type_groups ~modul state =
    let modul = get_module state modul in
    Linked_module.get_type_groups modul

  let fold_globals ~modul f acc state =
    let modul = get_module state modul in
    Linked_module.fold_globals f acc modul

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

    let value_to_gc_val (v : Concrete_value.t) : Concrete_ref.gc_val =
      match v with
      | I32 i -> I32 i
      | I64 i -> I64 i
      | F32 f -> F32 f
      | F64 f -> F64 f
      | V128 v -> V128 v
      | Ref r -> Ref r

    let default_gc_val (st : Binary.storage_type) : Concrete_ref.gc_val =
      match st with
      | Val_type (Num_type I32) -> I32 0l
      | Val_type (Num_type I64) -> I64 0L
      | Val_type (Num_type F32) -> F32 Float32.zero
      | Val_type (Num_type F64) -> F64 Float64.zero
      | Val_type (Num_type V128) -> V128 Concrete_v128.zero
      | Val_type (Ref_type (_, ht)) -> Ref (Concrete_ref.null ht)
      | Pack_type _ -> I32 0l

    let simple_instruction (types : Binary.sub_type array) modul stack =
      function
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
      | I31 Ref ->
        let i, stack = Stack.pop_i32 stack in
        Result.ok @@ Stack.push_ref stack (I31 i)
      | Struct (New id) ->
        let fields =
          match types.(id).ct with
          | Binary.Def_struct_t fl -> fl
          | _ -> Fmt.failwith "struct.new: type %d is not a struct type" id
        in
        let n = List.length fields in
        let top_n, stack = Stack.pop_n stack n in
        let fields = Array.of_list (List.rev_map value_to_gc_val top_n) in
        Result.ok @@ Stack.push_ref stack (Struct fields)
      | Struct (New_default id) ->
        let fields =
          match types.(id).ct with
          | Binary.Def_struct_t fl -> fl
          | _ ->
            Fmt.failwith "struct.new_default: type %d is not a struct type" id
        in
        let defaults =
          Array.of_list
            (List.map (fun (_, (_, st)) -> default_gc_val st) fields)
        in
        Result.ok @@ Stack.push_ref stack (Struct defaults)
      | Array (New _) ->
        let n, stack = Stack.pop_i32 stack in
        let v, stack = Stack.pop stack in
        let n = Int32.to_int n in
        let array = Array.make n (value_to_gc_val v) in
        Result.ok @@ Stack.push_ref stack (Array array)
      | Array (New_default id) ->
        let n, stack = Stack.pop_i32 stack in
        let st =
          match types.(id).ct with
          | Binary.Def_array_t (_, st) -> st
          | _ ->
            Fmt.failwith "array.new_default: type %d is not an array type" id
        in
        let n = Int32.to_int n in
        let array = Array.make n (default_gc_val st) in
        Result.ok @@ Stack.push_ref stack (Array array)
      | Array (New_fixed (_, n)) ->
        let n = Int32.to_int n in
        let top_n, stack = Stack.pop_n stack n in
        let array = Array.of_list (List.rev_map value_to_gc_val top_n) in
        Result.ok @@ Stack.push_ref stack (Array array)
      | Extern_convert_any ->
        let r, stack = Stack.pop_as_ref stack in
        let ref =
          match r with
          | NullRef -> Concrete_ref.Extern None
          | _ -> Concrete_ref.extern Concrete_ref.any_as_extern_key r
        in
        Result.ok @@ Stack.push_ref stack ref
      | Any_convert_extern ->
        let r, stack = Stack.pop_as_ref stack in
        let ref =
          match r with
          | Extern None -> Concrete_ref.NullRef
          | Extern (Some e) -> (
            match Concrete_ref.Extern.cast e Concrete_ref.any_as_extern_key with
            | Some inner -> inner
            | None -> Fmt.failwith "any.convert_extern: cast failure" )
          | _ -> Fmt.failwith "any.convert_extern: expected extern ref on stack"
        in
        Result.ok @@ Stack.push_ref stack ref
      | instr ->
        Fmt.failwith "TODO: Link: unimplemented instruction: %a"
          Binary.pp_simple_instruction instr

    let instr types modul stack instr =
      match instr.Annotated.raw with
      | Binary.Simple i -> simple_instruction types modul stack i
      | _ ->
        Fmt.failwith "TODO: Link: unimplemented instruction: %a"
          (Binary.pp_instr ~short:true)
          instr.Annotated.raw

    (* TODO: binary+const expr *)
    let expr types modul e : Concrete_value.t Result.t =
      let* stack =
        list_fold_left (instr types modul) Stack.empty e.Annotated.raw
      in
      match stack with
      | [] -> Error (`Type_mismatch "const expr returning zero values")
      | _ :: _ :: _ ->
        Error (`Type_mismatch "const expr returning more than one value")
      | [ result ] -> Ok result
  end

  let eval_global ls modul types
    (global : (Binary.Global.t, Binary.Global.Type.t) Origin.t) :
    global Result.t =
    match global with
    | Local global ->
      let* value = Eval_const.expr types modul global.init in
      let mut, typ = global.typ in
      let global : global = { value; mut; typ } in
      Ok global
    | Imported import -> load_global ls import

  let eval_globals ls modul types globals : Linked_module.Build.t Result.t =
    let+ modul, _i =
      array_fold_left
        (fun (modul, i) global ->
          let+ global = eval_global ls modul types global in
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

  let load_memory (ls : t) (import : Binary.Mem.Type.limits Origin.imported) :
    Concrete_memory.t Result.t =
    let* mem = load_from_module ls (fun (e : exports) -> e.memories) import in
    let imported_limit = Concrete_memory.get_limits mem in
    if memory_limit_is_included ~import:import.typ ~imported:imported_limit ()
    then Ok mem
    else Error (`Incompatible_import_type import.name)

  let eval_memory ls (memory : (Binary.Mem.t, Binary.Mem.Type.limits) Origin.t)
    : Concrete_memory.t Result.t =
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

  let load_table (ls : t) (import : Binary.Table.Type.t Origin.imported) :
    table Result.t =
    let typ : Binary.Table.Type.t = import.typ in
    let* t = load_from_module ls (fun (e : exports) -> e.tables) import in
    let imported_data_size = Concrete_table.size t in
    if table_types_are_compatible typ ~imported_data_size (t.limits, t.typ) then
      Ok t
    else Error (`Incompatible_import_type import.name)

  let eval_table ls (table : (Binary.Table.t, Binary.Table.Type.t) Origin.t) :
    table Result.t =
    match table with
    | Local { id = label; typ; _ } ->
      Result.ok @@ Concrete_table.init ?label typ
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

  let load_func (ls : t) (imp_types : Binary.sub_type array)
    (imp_type_groups : (int * int) array)
    (import : Binary.block_type Origin.imported) : func Result.t =
    let (Binary.Bt_raw (imp_idx_opt, typ)) = import.typ in
    let* func = load_from_module ls (fun (e : exports) -> e.functions) import in
    let compatible =
      match func with
      | Kind.Wasm { func; modul = idx } -> (
        let (Bt_raw (exp_idx_opt, type')) = func.type_f in
        let exp_modul = Dynarray.get ls.modules idx in
        let exp_types = Linked_module.get_types exp_modul in
        let exp_type_groups = Linked_module.get_type_groups exp_modul in
        match (imp_idx_opt, exp_idx_opt) with
        | Some imp_idx, Some exp_idx ->
          Binary.is_subtype exp_types exp_type_groups imp_types imp_type_groups
            ~got:exp_idx ~expected:imp_idx
        | _ -> Binary.func_type_eq typ type' )
      | Extern { idx } ->
        let _f, type' = Dynarray.get ls.extern_modules idx in
        Binary.func_type_eq typ type'
    in
    if compatible then Ok func
    else
      let (Binary.Bt_raw (_, type')) =
        match func with
        | Kind.Wasm { func; _ } -> func.type_f
        | Extern { idx } ->
          Binary.Bt_raw (None, snd (Dynarray.get ls.extern_modules idx))
      in
      let msg =
        Fmt.str "%s: expected: %a got: %a" import.name Binary.pp_func_type typ
          Binary.pp_func_type type'
      in
      Error (`Incompatible_import_type msg)

  let eval_func ls (modul : int) imp_types imp_type_groups func : func Result.t
      =
    match func with
    | Origin.Local func -> Result.ok @@ Kind.wasm func ~modul
    | Imported import -> load_func ls imp_types imp_type_groups import

  let eval_functions ls (finished_modul : int) imp_types imp_type_groups modul
    functions =
    let+ modul, _i =
      array_fold_left
        (fun (modul, i) func ->
          let+ func =
            eval_func ls finished_modul imp_types imp_type_groups func
          in
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
      let* tag = load_from_module ls (fun (e : exports) -> e.tags) import in
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
    [ Binary.Simple (I32 (Const offset))
    ; Simple (I32 (Const 0l))
    ; Simple (I32 (Const length))
    ; Simple (Table (Init (table, elem)))
    ; Simple (Elem (Drop elem))
    ]

  let active_data_expr modul ~offset ~length ~mem ~data =
    if not (IMap.mem mem (Linked_module.Build.get_memories modul)) then
      Error (`Unknown_memory (Text.Raw mem))
    else
      Ok
        [ Binary.Simple (I32 (Const offset))
        ; Simple (I32 (Const 0l))
        ; Simple (I32 (Const length))
        ; Simple (Memory (Init (mem, data)))
        ; Simple (Data (Drop data))
        ]

  let get_i32 = function
    | Concrete_value.I32 i -> Ok i
    | _ -> Error (`Type_mismatch "get_i32")

  let define_data types modul data =
    let+ modul, init, _i =
      array_fold_left
        (fun (modul, init, id) (data : Binary.Data.t) ->
          let modul =
            let data' = { Concrete_data.value = data.init } in
            Linked_module.Build.add_data id data' modul
          in
          let+ init =
            match data.mode with
            | Active (mem, offset) ->
              let* offset = Eval_const.expr types modul offset in
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

  let define_elem types modul elem =
    let+ modul, inits, _i =
      array_fold_left
        (fun (modul, inits, i) (elem : Binary.Elem.t) ->
          let* init = list_map (Eval_const.expr types modul) elem.init in
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
              let* offset = Eval_const.expr types modul offset in
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
    exports Result.t =
    let fill_exports get_value exports names =
      array_fold_left
        (fun (acc, names) ({ name; id; _ } : Binary.Export.t) ->
          let value = get_value modul id in
          if StringSet.mem name names then Error `Duplicate_export_name
          else Ok (StringMap.add name value acc, StringSet.add name names) )
        (StringMap.empty, names) exports
    in
    let names = StringSet.empty in
    let* globals, names =
      fill_exports Linked_module.get_global exports.global names
    in
    let* memories, names =
      fill_exports Linked_module.get_memory exports.mem names
    in
    let* tables, names =
      fill_exports Linked_module.get_table exports.table names
    in
    let* functions, names =
      fill_exports Linked_module.get_func exports.func names
    in
    let+ tags, names = fill_exports Linked_module.get_tag exports.tag names in
    { globals; memories; tables; functions; tags; defined_names = names }

  let link_binary_module ~name (ls : t) (binary_module : Binary.Module.t) =
    Log.info (fun m -> m "linking      ...");
    let ls = clone ls in
    let next_id = Dynarray.length ls.modules in
    let modul = Linked_module.Build.empty in
    let imp_type_groups =
      Binary.compute_type_groups binary_module.type_defs
        (Array.length binary_module.types)
    in
    let* modul =
      eval_functions ls next_id binary_module.types imp_type_groups modul
        binary_module.func
    in
    let* modul = eval_tags ls next_id modul binary_module.tag in
    let* modul =
      eval_globals ls modul binary_module.types binary_module.global
    in
    let* modul = eval_memories ls modul binary_module.mem in
    let* modul = eval_tables ls modul binary_module.table in
    let* modul, init_active_data =
      define_data binary_module.types modul binary_module.data
    in
    let* modul, init_active_elem =
      define_elem binary_module.types modul binary_module.elem
    in
    let init_code =
      let start =
        Option.map
          (fun start_id -> [ Binary.Call start_id ])
          binary_module.start
        |> Option.fold ~none:[] ~some:(fun s -> [ s ])
      in
      (init_active_elem @ init_active_data) @ start
      |> List.flatten |> Annotated.dummy_deep
    in

    let modul : Linked_module.t =
      Linked_module.freeze next_id modul init_code ls.extern_modules
        binary_module.types binary_module.type_defs
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

    ( next_id
    , { by_id
      ; by_name
      ; last = Some (by_id_exports, Linked_module.get_id modul)
      ; extern_modules = ls.extern_modules
      ; modules = ls.modules
      } )

  let link_extern_module ~name functions (ls : t) =
    let functions, extern_modules =
      List.fold_left
        (fun (functions, extern_modules) (name, func) ->
          let typ = M.to_func_type func in
          Dynarray.add_last extern_modules (func, typ);
          let id = Dynarray.length extern_modules - 1 in
          ((name, (Kind.extern id : Kind.func)) :: functions, extern_modules) )
        ([], ls.extern_modules) functions
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
      ; tags = StringMap.empty
      ; defined_names
      }
    in
    { ls with by_name = StringMap.add name exports ls.by_name; extern_modules }
end
