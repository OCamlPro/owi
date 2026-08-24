(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Allocator : sig
  type !+'a t

  type key

  val empty : 'a t

  val find_opt : key -> 'a t -> 'a option

  val add : 'a -> 'a t -> 'a t * key

  val add_manual : key -> 'a -> 'a t -> 'a t

  val next_key : 'a t -> key

  val succ_key : key -> key

  val plus_key : key -> int -> key

  val unsafe_to_int : key -> int

  val unsafe_of_int : int -> key

  val pp : 'a Fmt.t -> 'a t Fmt.t

  val pp_key : key Fmt.t
end = struct
  include Map.Make (Int)

  let next_key map = cardinal map

  let add_manual k v map = add k v map

  let add v map =
    let key = next_key map in
    let map = add key v map in
    (map, key)

  let succ_key key = succ key

  let plus_key k n = k + n

  let unsafe_to_int v = v

  let unsafe_of_int v = v

  let pp pp_v =
    Fmt.braces
      (Fmt.iter_bindings ~sep:Fmt.semi iter (fun ppf (k, v) ->
         Fmt.pf ppf "%d -> %a" k pp_v v ) )

  let pp_key ppf key = Fmt.pf ppf "%d" key
end

module IntMap = struct
  include Map.Make (Int)

  let pp pp_v =
    Fmt.braces
      (Fmt.iter_bindings ~sep:Fmt.semi iter (fun ppf (k, v) ->
         Fmt.pf ppf "%d -> %a" k pp_v v ) )
end

module StringMap = struct
  include Map.Make (String)

  let pp pp_v =
    Fmt.braces
      (Fmt.iter_bindings ~sep:Fmt.semi iter (fun ppf (k, v) ->
         Fmt.pf ppf "%S -> %a" k pp_v v ) )
end

module Make
    (Context : sig
      type t

      val empty : unit -> t
    end)
    (Value : sig
      type t

      val pp : t Fmt.t

      module Ref : sig
        module Extern : sig
          type t
        end

        type i32

        type 'value array_obj

        type 'value struct_obj

        type 'value t =
          | Extern of Extern.t option
          | Func of int option
          | NullExn
          | NullRef
          | I31 of i32
          | NullI31
          | Array of 'value array_obj
          | Struct of 'value struct_obj
          | ExternAsAny of Extern.t option
          | AnyAsExtern of 'value t
      end
    end)
    (Constexpr_eval :
      Constexpr_eval_intf.T
        with type value := Value.t
         and type reference := Value.t Value.Ref.t
         and type context := Context.t)
    (Memory : sig
      type t

      val get_limits : t -> Binary.Mem.Type.limits

      val init : Binary.Mem.Type.limits -> t
    end)
    (Table : sig
      type t

      val init : Binary.Table.Type.t -> t

      val size : t -> int

      (* TODO: could be stored at link time instead *)
      val get_type : t -> Binary.Table.Type.t
    end)
    (Elem : Elem_intf.T with type reference := Value.t Value.Ref.t)
    (Extern_func : sig
      type t

      val to_func_type : t -> Binary.func_type
    end)
    (Data : sig
      type t

      val of_string : string -> t
    end) :
  Env_intf.T
    with type extern_func := Extern_func.t
     and type value := Value.t
     and type elem := Elem.t
     and type data := Data.t
     and type table := Table.t
     and type memory := Memory.t
     and type context = Context.t = struct
  type modul = int

  type context = Context.t

  (* when evaluating constant expressions, we don't want to deal with value because building them is annoying and differs too much between the various interpreters, yet, the constant expression builders can read globals that could be values, but we use the fact that it can only read constant globals that are always going to be concrete, doing so allows us to have a single concrete implementation of constant evaluation, with the price of having to convert from concrete to {abstract,symbolic} each time we load a constant global, but who cares, we could simply inline them in the future and don't bother *)
  type global_value = Value.t

  let pp_global_value = Value.pp

  type global =
    { value : global_value
    ; typ : Binary.Global.Type.t
    }

  let pp_global ppf g = pp_global_value ppf g.value

  type t =
    { functions : Extern_func.t Kind.func Allocator.t
        (* map from runtime address to runtime functions *)
    ; globals : global Allocator.t
        (* map from runtime address to runtime globals *)
    ; memories : Memory.t Allocator.t
        (* map from runtime address to runtime memories *)
    ; tables : Table.t Allocator.t
        (* map from runtime address to runtime tables *)
    ; datas : Data.t Allocator.t (* map from runtime address to runtime datas *)
    ; elems : Elem.t Allocator.t (* map from runtime address to runtime elems *)
    ; tags :
        Binary.Tag.t Allocator.t (* map from runtime address to runtime tags *)
    ; initialization_codes : Binary.expr IntMap.t
        (* map from modul to their initialization code *)
    ; exported_functions : Allocator.key StringMap.t IntMap.t
        (* map from modul to their exported functions *)
    ; exported_globals : Allocator.key StringMap.t IntMap.t
        (* map from modul to their exported globals *)
    ; exported_memories : Allocator.key StringMap.t IntMap.t
        (* map from modul to their exported memories *)
    ; exported_tables : Allocator.key StringMap.t IntMap.t
        (* map from modul to their exported tables *)
    ; exported_tags : Allocator.key StringMap.t IntMap.t
    ; last_module : modul option (* last module that was added to the runtime *)
    ; registered_modules : modul StringMap.t
        (* map from registered names to modul *)
    ; context : Context.t
    ; raw_names : modul StringMap.t
        (* this is used only for scripts where modules can get a $id and we have to remember them to be able to register them this way... *)
    ; types : Binary.sub_type array
        (* table of all modules types (with the ids shifted) *)
    ; type_groups : (int * int) array
        (* table of all type groups (with their bound ids shifted) *)
    }

  let pp ppf
    { functions
    ; globals
    ; memories
    ; tables
    ; datas
    ; elems
    ; tags = _
    ; initialization_codes
    ; exported_functions
    ; exported_globals
    ; exported_memories
    ; exported_tables
    ; exported_tags = _
    ; last_module
    ; registered_modules
    ; context = _
    ; raw_names = _
    ; types
    ; type_groups
    } =
    let pp_todo ppf _v = Fmt.pf ppf "<TODO>" in
    let pp_elem = pp_todo in
    let pp_table = pp_todo in
    let pp_memory = pp_todo in
    let pp_data = pp_todo in
    let pp_modul ppf v = Fmt.pf ppf "%d" v in
    let pp_types = pp_todo in
    let pp_type_groups = pp_todo in
    Fmt.pf ppf
      "@[<v>functions: %a@,\
       globals: %a@,\
       memories: %a@,\
       tables: %a@,\
       datas: %a@,\
       elems: %a@,\
       initialization_codes: %a@,\
       exported_functions: %a@,\
       exported_globals: %a@,\
       exported_memories: %a@,\
       exported_tables: %a@,\
       last_module: %a@,\
       registered_modules: %a@,\
       types: %a@,\
       type_groups: %a@]"
      (Allocator.pp Kind.pp_func)
      functions (Allocator.pp pp_global) globals (Allocator.pp pp_memory)
      memories (Allocator.pp pp_table) tables (Allocator.pp pp_data) datas
      (Allocator.pp pp_elem) elems
      (IntMap.pp (fun ppf e ->
         Binary.pp_expr ~short:true ppf (Annotated.dummy e) ) )
      initialization_codes
      (IntMap.pp (StringMap.pp Allocator.pp_key))
      exported_functions
      (IntMap.pp (StringMap.pp Allocator.pp_key))
      exported_globals
      (IntMap.pp (StringMap.pp Allocator.pp_key))
      exported_memories
      (IntMap.pp (StringMap.pp Allocator.pp_key))
      exported_tables (Fmt.option pp_modul) last_module (StringMap.pp pp_modul)
      registered_modules pp_types types pp_type_groups type_groups

  let empty =
    let functions = Allocator.empty in
    let globals = Allocator.empty in
    let memories = Allocator.empty in
    let tables = Allocator.empty in
    let datas = Allocator.empty in
    let elems = Allocator.empty in
    let tags = Allocator.empty in
    let initialization_codes = IntMap.empty in
    let exported_functions = IntMap.empty in
    let exported_globals = IntMap.empty in
    let exported_memories = IntMap.empty in
    let exported_tables = IntMap.empty in
    let exported_tags = IntMap.empty in
    let last_module = None in
    let registered_modules = StringMap.empty in
    let context = Context.empty () in
    let raw_names = StringMap.empty in
    let types = [||] in
    let type_groups = [||] in
    { functions
    ; globals
    ; memories
    ; tables
    ; datas
    ; elems
    ; tags
    ; initialization_codes
    ; exported_functions
    ; exported_globals
    ; exported_memories
    ; exported_tables
    ; exported_tags
    ; last_module
    ; registered_modules
    ; context
    ; raw_names
    ; types
    ; type_groups
    }

  let get_last_module ~env =
    match env.last_module with
    | None -> Error (`Unknown_module "there was no last module")
    | Some modul -> Ok modul

  let register_module ~env ~name ~modid =
    let+ modul =
      match modid with
      | None -> get_last_module ~env
      | Some id ->
        begin match StringMap.find_opt id env.raw_names with
        | None -> Error (`Unknown_module id)
        | Some id -> Ok id
        end
    in
    let registered_modules = StringMap.add name modul env.registered_modules in
    { env with registered_modules }

  let get_registered_module ~env ~name =
    match StringMap.find_opt name env.registered_modules with
    | None -> Error (`Unknown_module name)
    | Some modul -> Ok modul

  let get_next_module ~env =
    match env.last_module with None -> 0 | Some modul -> succ modul

  let get_initialization_code ~env ~modul : Binary.expr =
    match IntMap.find_opt modul env.initialization_codes with
    | Some expr -> expr
    | None -> []

  let load_exported_key exported ~env ~modul_name ~name =
    (* find the source module *)
    let* modul = get_registered_module ~env ~name:modul_name in
    let not_found () =
      (* if there is something else exported with this name, then, we must fail with a incompatible import type instead... *)
      let exists_somewhere_else =
        [ IntMap.find_opt modul env.exported_tables
        ; IntMap.find_opt modul env.exported_memories
        ; IntMap.find_opt modul env.exported_globals
        ; IntMap.find_opt modul env.exported_functions
        ; IntMap.find_opt modul env.exported_tags
        ]
        |> List.map (function
          | None -> None
          | Some names -> StringMap.find_opt name names )
        |> List.exists Option.is_some
      in
      if exists_somewhere_else then Error (`Incompatible_import_type name)
      else Error (`Unknown_import (modul_name, name))
    in
    (* finc the exports for this module *)
    match IntMap.find_opt modul exported with
    | None ->
      (* it should be there! *)
      not_found ()
    | Some names ->
      (* find the address for the export with the desired name *)
      begin match StringMap.find_opt name names with
      | None -> not_found ()
      | Some address -> Ok address
      end

  let load_import ~env ~import:({ modul_name; name; _ } : _ Origin.imported)
    exported allocator =
    (* find the address of the map *)
    let* address = load_exported_key exported ~env ~modul_name ~name in
    (* find its env value *)
    match Allocator.find_opt address allocator with
    | None ->
      (* it should be there! *)
      assert false
    | Some func -> Ok (func, address)

  let link_function ~env ~type_base_id id (functions, map) = function
    | Origin.Local func ->
      let func : Extern_func.t Kind.func = Kind.Wasm func in
      let address = Allocator.plus_key (Allocator.next_key env.functions) id in
      let functions = (address, func) :: functions in
      let map = IntMap.add id address map in
      Ok (functions, map)
    | Imported ({ name; typ; _ } as import) ->
      let* func, address =
        load_import ~env ~import env.exported_functions env.functions
      in
      (* comparing their types *)
      let* () =
        let expected_type_id =
          Option.map (fun id -> id + type_base_id) (fst typ)
        in
        let _, expected_ft = typ in
        let got_type_id, got_ft =
          match (func : Extern_func.t Kind.func) with
          | Kind.Wasm f -> f.type_f
          | Kind.Extern f -> (None, Extern_func.to_func_type f)
        in
        let type_matches =
          match (expected_type_id, got_type_id) with
          | Some expected, Some got ->
            Binary.is_subtype env.types env.type_groups env.types
              env.type_groups ~got ~expected
          | _ -> Binary.func_type_eq expected_ft got_ft
        in
        if type_matches then Ok ()
        else
          let msg =
            Fmt.str "%s: expected: %a got: %a" name Binary.pp_func_type
              expected_ft Binary.pp_func_type got_ft
          in
          Error (`Incompatible_import_type msg)
      in
      (* adding new global to the address map *)
      let map = IntMap.add id address map in
      Ok (functions, map)

  let link_global ctx ~get_const_type ~get_const_func ~get_const_global ~env id
    ((globals : (Allocator.key * global) list), map) = function
    | Origin.Local ({ init; typ; id = _ } : Binary.Global.t) ->
      let* value =
        Constexpr_eval.expr ctx ~get_const_type ~get_const_func
          ~get_const_global init.raw
      in
      let global : global = { value; typ } in

      let address = Allocator.plus_key (Allocator.next_key env.globals) id in
      let globals = (address, global) :: globals in

      let map = IntMap.add id address map in
      Ok (globals, map)
    | Imported ({ name; typ; _ } as import) ->
      let* global, address =
        load_import ~env ~import env.exported_globals env.globals
      in

      (* comparing their types *)
      let* () =
        match (global.typ, typ) with
        | (Var, _), ((Const : Text.mut), _) | (Const, _), (Var, _) ->
          Error (`Incompatible_import_type name)
        | (Var, t1), (Var, t2) ->
          if Binary.val_type_eq t1 t2 then Ok ()
          else Error (`Incompatible_import_type name)
        | (Const, t1), (Const, t2) ->
          if Binary.is_subtype_val_type t1 t2 then Ok ()
          else Error (`Incompatible_import_type name)
      in
      (* adding new global to the address map *)
      let map = IntMap.add id address map in
      Ok (globals, map)

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

  let table_types_are_compatible ~imported_data_size
    (import, (t1 : Binary.ref_type)) (imported, t2) =
    table_limit_is_included ~imported_data_size ~import ~imported ()
    && Binary.ref_type_eq t1 t2

  let link_memory ~env id (memories, map) = function
    | Origin.Local (_label, typ) ->
      let memory = Memory.init typ in

      let address = Allocator.plus_key (Allocator.next_key env.memories) id in
      let memories = (address, memory) :: memories in

      let map = IntMap.add id address map in
      Ok (memories, map)
    | Imported ({ name; typ; _ } as import) ->
      let* memory, address =
        load_import ~env ~import env.exported_memories env.memories
      in
      (* comparing their types *)
      let* () =
        let imported_limit = Memory.get_limits memory in
        if memory_limit_is_included ~import:typ ~imported:imported_limit () then
          Ok ()
        else Error (`Incompatible_import_type name)
      in
      (* adding new memory to the address map *)
      let map = IntMap.add id address map in
      Ok (memories, map)

  let link_table ~env id (tables, map) = function
    | Origin.Local { Binary.Table.typ; _ } ->
      let table = Table.init typ in

      let address = Allocator.plus_key (Allocator.next_key env.tables) id in
      let tables = (address, table) :: tables in

      let map = IntMap.add id address map in
      Ok (tables, map)
    | Imported ({ name; typ; _ } as import) ->
      let* table, address =
        load_import ~env ~import env.exported_tables env.tables
      in
      (* comparing their types *)
      let* () =
        let imported_data_size = Table.size table in
        let typ' = Table.get_type table in
        if table_types_are_compatible typ typ' ~imported_data_size then Ok ()
        else Error (`Incompatible_import_type name)
      in
      (* adding new table to the address map *)
      let map = IntMap.add id address map in
      Ok (tables, map)

  let link_data ~env ~memories_map id
    ((initialization_code : Binary.expr), datas, map)
    { Binary.Data.init; mode; _ } =
    let data = init in

    let address = Allocator.plus_key (Allocator.next_key env.datas) id in
    let datas = (address, data) :: datas in

    let map = IntMap.add id address map in
    let* initialization_code =
      match mode with
      | Passive -> Ok initialization_code
      | Active (mem, offset) ->
        begin match IntMap.find_opt mem memories_map with
        | None -> Error (`Unknown_memory (Text.Raw mem))
        | Some _ ->
          let length = String.length init |> Concrete_i32.of_int in
          (* Jean-Christophe, I'm sorry for writing this, please forgive me... *)
          Ok
            ( initialization_code @ offset.raw
            @ Annotated.dummies
                [ Binary.Simple (I32 (Const 0l))
                ; Simple (I32 (Const length))
                ; Simple (Memory (Init (mem, id)))
                ; Simple (Data (Drop id))
                ] )
        end
    in
    Ok (initialization_code, datas, map)

  let link_elem ctx ~get_const_type ~get_const_func ~get_const_global ~env id
    (initialization_code, elems, map) { Binary.Elem.init; mode; _ } =
    let* init =
      list_map
        (fun expr ->
          Constexpr_eval.ref_expr ctx ~get_const_type ~get_const_func
            ~get_const_global expr.Annotated.raw )
        init
    in
    let elem =
      match mode with
      | Declarative -> (* Declarative elements have no env value *) []
      | Active _ | Passive -> init
    in

    let address = Allocator.plus_key (Allocator.next_key env.elems) id in
    let elems = (address, elem) :: elems in

    let map = IntMap.add id address map in
    match mode with
    | Passive | Declarative -> Ok (initialization_code, elems, map)
    | Active (table, offset) ->
      let length = Int32.of_int @@ List.length init in
      let initialization_code =
        initialization_code @ offset.raw
        @ Annotated.dummies
            [ Binary.Simple (I32 (Const 0l))
            ; Simple (I32 (Const length))
            ; Simple (Table (Init (table, id)))
            ; Simple (Elem (Drop id))
            ]
      in
      Ok (initialization_code, elems, map)

  let link_tag ~env id (tags, map) = function
    | Origin.Local tag ->
      let address = Allocator.plus_key (Allocator.next_key env.tables) id in
      let tags = (address, tag) :: tags in
      let map = IntMap.add id address map in
      Ok (tags, map)
    | Imported ({ name; typ; _ } as import) ->
      let* tag, address = load_import ~env ~import env.exported_tags env.tags in
      (* comparing their types *)
      let _, typ = typ in
      let _, actual_type = tag.typ in
      let* () =
        if Binary.func_type_eq typ actual_type then Ok ()
        else
          let msg =
            Fmt.str "%s: expected: %a got: %a" name Binary.pp_func_type typ
              Binary.pp_func_type actual_type
          in
          Error (`Incompatible_import_type msg)
      in
      (* adding new table to the address map *)
      let map = IntMap.add id address map in
      Ok (tags, map)

  let link_binary_module ~(env : t) ~name ~(modul : Binary.Module.t) :
    t Result.t =
    Log.info (fun m -> m "linking      ...");
    (* This is the first step where we simply allocate the env values for functions, globals, memories etc.
                                             Each one is given a unique address in a global space, and we maintain a map from (module id, {func,global,...} id) to env address. *)
    let new_module = get_next_module ~env in
    (* type_base_id: the number all previously identified types from the modules
       that were treated before *)
    let type_base_id = Array.length env.types in
    let rewrite_type_id id = type_base_id + id in
    let rewrite_heap_type : Binary.heap_type -> Binary.heap_type = function
      | TypeUse id -> TypeUse (rewrite_type_id id)
      | ht -> ht
    in
    let rewrite_ref_type : Binary.ref_type -> Binary.ref_type =
     fun (nullable, ht) -> (nullable, rewrite_heap_type ht)
    in
    let rewrite_val_type : Binary.val_type -> Binary.val_type = function
      | Ref_type rt -> Ref_type (rewrite_ref_type rt)
      | vt -> vt
    in
    let rewrite_storage_type : Binary.storage_type -> Binary.storage_type =
      function
      | Val_type vt -> Val_type (rewrite_val_type vt)
      | Pack_type _ as pt -> pt
    in
    let rewrite_field_type : Binary.field_type -> Binary.field_type =
     fun (mut, st) -> (mut, rewrite_storage_type st)
    in
    let rewrite_comp_type : Binary.comp_type -> Binary.comp_type = function
      | Def_struct_t fields ->
        Def_struct_t
          (List.map (fun (id, ft) -> (id, rewrite_field_type ft)) fields)
      | Def_array_t ft -> Def_array_t (rewrite_field_type ft)
      | Def_func_t (params, results) ->
        Def_func_t
          ( List.map (fun (id, vt) -> (id, rewrite_val_type vt)) params
          , List.map rewrite_val_type results )
    in
    let rewrite_sub_type : Binary.sub_type -> Binary.sub_type =
     fun { final; ids; ct } ->
      { final; ids = List.map rewrite_type_id ids; ct = rewrite_comp_type ct }
    in
    let types = Array.map rewrite_sub_type modul.types in
    let type_groups =
      let module_groups =
        Binary.compute_type_groups modul.type_defs (Array.length modul.types)
      in
      Array.map (fun (lo, size) -> (rewrite_type_id lo, size)) module_groups
    in
    (* We need to compute the updated environment with the new types and type
       groups earlier so that we can use it for imported functions *)
    let env =
      { env with
        types = Array.append env.types types
      ; type_groups = Array.append env.type_groups type_groups
      }
    in
    (* TODO: should it be passed to the function instead? *)
    let ctx = Context.empty () in
    (* functions *)
    let* functions, functions_map =
      array_fold_lefti
        (link_function ~env ~type_base_id)
        ([], IntMap.empty) modul.func
    in
    (* tags *)
    (* TODO *)

    let get_const_global ~env globals globals_map id =
      (* we should only make visible previously defined immutable globals and imported immutable globals. *)
      match IntMap.find_opt id globals_map with
      | None -> assert false
      | Some address ->
        begin match List.assoc_opt address globals with
        | Some g -> Ok g.value
        | None ->
          begin match Allocator.find_opt address env.globals with
          | Some g -> Ok g.value
          | None -> assert false
          end
        end
    in

    let get_const_func functions_map id =
      (* we should only make visible functions that are defined locally, not imported functions *)
      (* TODO: this can probably be changed to remove the Result wrap? *)
      match IntMap.find_opt id functions_map with
      | None -> assert false
      | Some address -> Ok (Allocator.unsafe_to_int address)
    in

    (* TODO! *)
    let get_const_type _id = assert false in

    (* globals *)
    let* globals, globals_map =
      array_fold_lefti
        (fun id ((globals : (Allocator.key * global) list), map) ->
          link_global ctx ~get_const_type
            ~get_const_func:(get_const_func functions_map)
            ~get_const_global:(get_const_global ~env globals map)
            ~env id (globals, map) )
        ([], IntMap.empty) modul.global
    in
    (* memories *)
    let* memories, memories_map =
      array_fold_lefti (link_memory ~env) ([], IntMap.empty) modul.mem
    in
    (* tables *)
    let* tables, tables_map =
      array_fold_lefti (link_table ~env) ([], IntMap.empty) modul.table
    in
    (* tags *)
    (* TODO: rewrite tags later using tags_map, it has not been done for now... *)
    let* tags, tags_map =
      array_fold_lefti (link_tag ~env) ([], IntMap.empty) modul.tag
    in
    (* initialization code *)
    (* 1. data *)
    let* initialization_code, datas, datas_map =
      array_fold_lefti
        (link_data ~env ~memories_map)
        ([], [], IntMap.empty) modul.data
    in
    (* 2. elem *)
    let* initialization_code, elems, elems_map =
      array_fold_lefti
        (link_elem ctx ~get_const_type
           ~get_const_func:(get_const_func functions_map)
           ~get_const_global:(get_const_global ~env globals globals_map)
           ~env )
        (initialization_code, [], IntMap.empty)
        modul.elem
    in
    (* 3. start function *)
    (* TODO *)
    let initialization_code =
      match modul.Binary.Module.start with
      | None -> initialization_code
      | Some func ->
        initialization_code @ [ Annotated.dummy (Binary.Call func) ]
    in

    (* Now this is the second step, where we rewrite all access to use env address.
                                     For instance, if a function contains the instruction global.get 0, the 0 is local to the modul in which the function is defined.
                                     We look what is the env address of this global in the map, by looking the global map at (module_id, 0).
                                     If the env address is say, 42, we rewrite the instruction to be global.get 42. *)
    let get_unsafe k tbl =
      match IntMap.find_opt k tbl with
      | Some v -> Allocator.unsafe_to_int v
      | None -> assert false
    in
    let rewrite_global_instruction : Binary.global_instr -> Binary.global_instr
        = function
      | Get i -> Get (get_unsafe i globals_map)
      | Set i -> Set (get_unsafe i globals_map)
    in
    let rewrite_i32_instruction : Binary.i32_instr -> Binary.i32_instr =
      function
      | ( Const _ | Clz | Ctz | Popcnt | Add | Sub | Mul | Div_s | Div_u | Rem_s
        | Rem_u | And | Or | Xor | Shl | Shr_s | Shr_u | Rotl | Rotr | Eqz | Eq
        | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u | Ge_s | Ge_u | Extend8_s
        | Extend16_s | Wrap_i64 | Trunc_f_s _ | Trunc_f_u _ | Trunc_sat_f_s _
        | Trunc_sat_f_u _ | Reinterpret_f _ ) as i ->
        i
      | Load (i, memarg) -> Load (get_unsafe i memories_map, memarg)
      | Load8_s (i, memarg) -> Load8_s (get_unsafe i memories_map, memarg)
      | Load8_u (i, memarg) -> Load8_u (get_unsafe i memories_map, memarg)
      | Load16_s (i, memarg) -> Load16_s (get_unsafe i memories_map, memarg)
      | Load16_u (i, memarg) -> Load16_u (get_unsafe i memories_map, memarg)
      | Store (i, memarg) -> Store (get_unsafe i memories_map, memarg)
      | Store8 (i, memarg) -> Store8 (get_unsafe i memories_map, memarg)
      | Store16 (i, memarg) -> Store16 (get_unsafe i memories_map, memarg)
    in
    let rewrite_i64_instruction : Binary.i64_instr -> Binary.i64_instr =
      function
      | ( Const _ | Clz | Ctz | Popcnt | Add | Sub | Mul | Div_s | Div_u | Rem_s
        | Rem_u | And | Or | Xor | Shl | Shr_s | Shr_u | Rotl | Rotr | Eqz | Eq
        | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u | Ge_s | Ge_u | Extend8_s
        | Extend16_s | Trunc_f_s _ | Trunc_f_u _ | Trunc_sat_f_s _
        | Trunc_sat_f_u _ | Reinterpret_f _ | Extend32_s | Extend_i32_s
        | Extend_i32_u ) as i ->
        i
      | Load (i, memarg) -> Load (get_unsafe i memories_map, memarg)
      | Load8_s (i, memarg) -> Load8_s (get_unsafe i memories_map, memarg)
      | Load8_u (i, memarg) -> Load8_u (get_unsafe i memories_map, memarg)
      | Load16_s (i, memarg) -> Load16_s (get_unsafe i memories_map, memarg)
      | Load16_u (i, memarg) -> Load16_u (get_unsafe i memories_map, memarg)
      | Load32_s (i, memarg) -> Load32_s (get_unsafe i memories_map, memarg)
      | Load32_u (i, memarg) -> Load32_u (get_unsafe i memories_map, memarg)
      | Store (i, memarg) -> Store (get_unsafe i memories_map, memarg)
      | Store8 (i, memarg) -> Store8 (get_unsafe i memories_map, memarg)
      | Store16 (i, memarg) -> Store16 (get_unsafe i memories_map, memarg)
      | Store32 (i, memarg) -> Store32 (get_unsafe i memories_map, memarg)
    in
    let rewrite_f32_instruction : Binary.f32_instr -> Binary.f32_instr =
      function
      | ( Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest | Sub | Mul | Div
        | Min | Max | Copysign | Eq | Ne | Lt | Gt | Le | Ge | Demote_f64
        | Const _ | Convert_i_s _ | Convert_i_u _ | Reinterpret_i _ | Add ) as i
        ->
        i
      | Load (i, memarg) -> Load (get_unsafe i memories_map, memarg)
      | Store (i, memarg) -> Store (get_unsafe i memories_map, memarg)
    in
    let rewrite_f64_instruction : Binary.f64_instr -> Binary.f64_instr =
      function
      | ( Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest | Add | Sub | Mul
        | Div | Min | Max | Copysign | Eq | Ne | Lt | Gt | Le | Ge | Promote_f32
        | Const _ | Convert_i_s _ | Convert_i_u _ | Reinterpret_i _ ) as i ->
        i
      | Load (i, memarg) -> Load (get_unsafe i memories_map, memarg)
      | Store (i, memarg) -> Store (get_unsafe i memories_map, memarg)
    in
    let rewrite_v128_instruction : Binary.v128_instr -> Binary.v128_instr =
      function
      | (And | Not | Or | Any_true | Bitselect | Xor | Andnot | Const _) as i ->
        i
      | Load8_splat (i, memarg) ->
        Load8_splat (get_unsafe i memories_map, memarg)
      | Load8_lane (i, memarg, n) ->
        Load8_lane (get_unsafe i memories_map, memarg, n)
      | Load8x8_s (i, memarg) -> Load8x8_s (get_unsafe i memories_map, memarg)
      | Load8x8_u (i, memarg) -> Load8x8_u (get_unsafe i memories_map, memarg)
      | Load16_splat (i, memarg) ->
        Load16_splat (get_unsafe i memories_map, memarg)
      | Load16_lane (i, memarg, n) ->
        Load16_lane (get_unsafe i memories_map, memarg, n)
      | Load16x4_s (i, memarg) -> Load16x4_s (get_unsafe i memories_map, memarg)
      | Load16x4_u (i, memarg) -> Load16x4_u (get_unsafe i memories_map, memarg)
      | Load32_splat (i, memarg) ->
        Load32_splat (get_unsafe i memories_map, memarg)
      | Load32_lane (i, memarg, n) ->
        Load32_lane (get_unsafe i memories_map, memarg, n)
      | Load32_zero (i, memarg) ->
        Load32_zero (get_unsafe i memories_map, memarg)
      | Load64_splat (i, memarg) ->
        Load64_splat (get_unsafe i memories_map, memarg)
      | Load64_lane (i, memarg, n) ->
        Load64_lane (get_unsafe i memories_map, memarg, n)
      | Load64_zero (i, memarg) ->
        Load64_zero (get_unsafe i memories_map, memarg)
      | Load (i, memarg) -> Load (get_unsafe i memories_map, memarg)
      | Store (i, memarg) -> Store (get_unsafe i memories_map, memarg)
      | Store8_lane (i, memarg, n) ->
        Store8_lane (get_unsafe i memories_map, memarg, n)
      | Store64_lane (i, memarg, n) ->
        Store64_lane (get_unsafe i memories_map, memarg, n)
      | Store32_zero (i, memarg) ->
        Store32_zero (get_unsafe i memories_map, memarg)
      | Store32_lane (i, memarg, n) ->
        Store32_lane (get_unsafe i memories_map, memarg, n)
      | Store16_lane (i, memarg, n) ->
        Store16_lane (get_unsafe i memories_map, memarg, n)
      | Load32x2_s (i, memarg) -> Load32x2_s (get_unsafe i memories_map, memarg)
      | Load32x2_u (i, memarg) -> Load32x2_u (get_unsafe i memories_map, memarg)
    in
    let rewrite_i8x16_instruction : Text.i8x16_instr -> Text.i8x16_instr =
      function
      | ( Add | Sub | Eq | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u | Ge_s
        | Ge_u | Abs | Neg | Popcnt | All_true | Bitmask | Swizzle | Splat | Shl
        | Shr_s | Shr_u | Min_s | Min_u | Add_sat_s | Add_sat_u | Sub_sat_s
        | Sub_sat_u | Max_s | Max_u | Narrow_i16x8_s | Narrow_i16x8_u | Avgr_u
        | Shuffle _ | Extract_lane_s _ | Extract_lane_u _ | Replace_lane _ ) as
        i ->
        i
    in
    let rewrite_i16x8_instruction : Text.i16x8_instr -> Text.i16x8_instr =
      function
      | ( Add | Sub | Mul | Eq | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u
        | Ge_s | Ge_u | Splat | Q15mulr_sat_s | Min_s | Min_u
        | Extmul_low_i8x16_s | Extmul_low_i8x16_u | Extmul_high_i8x16_s
        | Extmul_high_i8x16_u | Extend_low_i8x16_s | Extend_low_i8x16_u
        | Extend_high_i8x16_s | Extend_high_i8x16_u | Extadd_pairwise_i8x16_s
        | Extadd_pairwise_i8x16_u | Add_sat_s | Add_sat_u | Sub_sat_s
        | Sub_sat_u | Max_s | Max_u | Shl | Neg | All_true | Shr_s | Shr_u
        | Bitmask | Avgr_u | Abs | Narrow_i32x4_s | Narrow_i32x4_u
        | Extract_lane_s _ | Extract_lane_u _ | Replace_lane _ ) as i ->
        i
    in
    let rewrite_i32x4_instruction : Text.i32x4_instr -> Text.i32x4_instr =
      function
      | ( Add | Sub | Mul | Shl | Shr_s | Shr_u | Eq | Ne | Lt_s | Lt_u | Gt_s
        | Gt_u | Le_s | Le_u | Ge_s | Ge_u | Splat | Extend_low_i16x8_s
        | Extend_high_i16x8_s | Extend_low_i16x8_u | Extend_high_i16x8_u
        | Trunc_sat_f64x2_s_zero | Trunc_sat_f64x2_u_zero | Trunc_sat_f32x4_s
        | Trunc_sat_f32x4_u | Min_s | Min_u | Extmul_low_i16x8_s
        | Extmul_low_i16x8_u | Extmul_high_i16x8_s | Extmul_high_i16x8_u
        | Extadd_pairwise_i16x8_s | Extadd_pairwise_i16x8_u | Dot_i16x8_s | Neg
        | Max_s | Max_u | Abs | All_true | Bitmask | Extract_lane _
        | Replace_lane _ ) as i ->
        i
    in
    let rewrite_i64x2_instruction : Text.i64x2_instr -> Text.i64x2_instr =
      function
      | ( Add | Sub | Mul | Eq | Ne | Lt_s | Gt_s | Le_s | Ge_s | Splat
        | Extend_low_i32x4_s | Extend_low_i32x4_u | Extend_high_i32x4_s
        | Extend_high_i32x4_u | Extmul_low_i32x4_s | Extmul_low_i32x4_u
        | Extmul_high_i32x4_s | Extmul_high_i32x4_u | Abs | Neg | All_true
        | Bitmask | Shl | Shr_s | Shr_u | Extract_lane _ | Replace_lane _ ) as i
        ->
        i
    in
    let rewrite_f32x4_instruction : Text.f32x4_instr -> Text.f32x4_instr =
      function
      | ( Add | Pmin | Min | Eq | Convert_i32x4_s | Convert_i32x4_u | Ceil | Max
        | Floor | Pmax | Ne | Sub | Abs | Trunc | Lt | Gt | Le | Ge | Mul
        | Convert_low_i32x4_s | Convert_low_i32x4_u | Convert_high_i32x4_s
        | Convert_high_i32x4_u | Splat | Nearest | Div | Neg | Sqrt
        | Demote_f64x2_zero | Extract_lane _ | Replace_lane _ ) as i ->
        i
    in
    let rewrite_f64x2_instruction : Text.f64x2_instr -> Text.f64x2_instr =
      function
      | ( Add | Pmin | Min | Eq | Ceil | Max | Floor | Pmax | Ne | Sub | Abs
        | Trunc | Lt | Gt | Le | Ge | Mul | Convert_low_i32x4_s
        | Convert_low_i32x4_u | Convert_high_i32x4_s | Convert_high_i32x4_u
        | Nearest | Div | Neg | Sqrt | Splat | Promote_low_f32x4
        | Extract_lane _ | Replace_lane _ ) as i ->
        i
    in
    let rewrite_ref_instruction : Binary.ref_instr -> Binary.ref_instr =
      function
      | Null ht -> Null (rewrite_heap_type ht)
      | Test rt -> Test (rewrite_ref_type rt)
      | Cast rt -> Cast (rewrite_ref_type rt)
      | (Is_null | As_non_null | Eq) as i -> i
      | Func i -> Func (get_unsafe i functions_map)
    in
    let rewrite_table_instruction : Binary.table_instr -> Binary.table_instr =
      function
      | Get i -> Get (get_unsafe i tables_map)
      | Set i -> Set (get_unsafe i tables_map)
      | Size i -> Size (get_unsafe i tables_map)
      | Grow i -> Grow (get_unsafe i tables_map)
      | Fill i -> Fill (get_unsafe i tables_map)
      | Copy (i1, i2) ->
        Copy (get_unsafe i1 tables_map, get_unsafe i2 tables_map)
      | Init (i1, i2) -> Init (get_unsafe i1 tables_map, get_unsafe i2 elems_map)
    in
    let rewrite_elem_instruction : Binary.elem_instr -> Binary.elem_instr =
      function
      | Drop i -> Drop (get_unsafe i elems_map)
    in
    let rewrite_memory_instruction : Binary.memory_instr -> Binary.memory_instr
        = function
      | Size i -> Size (get_unsafe i memories_map)
      | Grow i -> Grow (get_unsafe i memories_map)
      | Fill i -> Fill (get_unsafe i memories_map)
      | Copy (i1, i2) ->
        Copy (get_unsafe i1 memories_map, get_unsafe i2 memories_map)
      | Init (i1, i2) ->
        Init (get_unsafe i1 memories_map, get_unsafe i2 datas_map)
    in
    let rewrite_data_instruction : Binary.data_instr -> Binary.data_instr =
      function
      | Drop i -> Drop (get_unsafe i datas_map)
    in
    let rewrite_struct_instruction : Binary.struct_instr -> Binary.struct_instr
        = function
      | New id -> New (rewrite_type_id id)
      | New_default id -> New_default (rewrite_type_id id)
      | Get (ty, fld) -> Get (rewrite_type_id ty, fld)
      | Get_s (ty, fld) -> Get_s (rewrite_type_id ty, fld)
      | Get_u (ty, fld) -> Get_u (rewrite_type_id ty, fld)
      | Set (ty, fld) -> Set (rewrite_type_id ty, fld)
    in
    let rewrite_array_instruction : Binary.array_instr -> Binary.array_instr =
      function
      | New id -> New (rewrite_type_id id)
      | New_default id -> New_default (rewrite_type_id id)
      | New_fixed (id, n) -> New_fixed (rewrite_type_id id, n)
      | New_data (ty, data) ->
        New_data (rewrite_type_id ty, get_unsafe data datas_map)
      | New_elem (ty, elem) ->
        New_elem (rewrite_type_id ty, get_unsafe elem elems_map)
      | Get id -> Get (rewrite_type_id id)
      | Get_s id -> Get_s (rewrite_type_id id)
      | Get_u id -> Get_u (rewrite_type_id id)
      | Set id -> Set (rewrite_type_id id)
      | Fill id -> Fill (rewrite_type_id id)
      | Copy (id1, id2) -> Copy (rewrite_type_id id1, rewrite_type_id id2)
      | Init_data (ty, data) ->
        Init_data (rewrite_type_id ty, get_unsafe data datas_map)
      | Init_elem (ty, elem) ->
        Init_elem (rewrite_type_id ty, get_unsafe elem elems_map)
      | Len as i -> i
    in
    let rewrite_simple_instruction :
      Binary.simple_instruction -> Binary.simple_instruction = function
      | Global i -> Global (rewrite_global_instruction i)
      | I32 i -> I32 (rewrite_i32_instruction i)
      | I64 i -> I64 (rewrite_i64_instruction i)
      | F32 i -> F32 (rewrite_f32_instruction i)
      | F64 i -> F64 (rewrite_f64_instruction i)
      | V128 i -> V128 (rewrite_v128_instruction i)
      | I8x16 i -> I8x16 (rewrite_i8x16_instruction i)
      | I16x8 i -> I16x8 (rewrite_i16x8_instruction i)
      | I32x4 i -> I32x4 (rewrite_i32x4_instruction i)
      | I64x2 i -> I64x2 (rewrite_i64x2_instruction i)
      | F32x4 i -> F32x4 (rewrite_f32x4_instruction i)
      | F64x2 i -> F64x2 (rewrite_f64x2_instruction i)
      | Ref i -> Ref (rewrite_ref_instruction i)
      | Table i -> Table (rewrite_table_instruction i)
      | Elem i -> Elem (rewrite_elem_instruction i)
      | Memory i -> Memory (rewrite_memory_instruction i)
      | Data i -> Data (rewrite_data_instruction i)
      | ( Nop | Local _ | Drop | Unreachable | Any_convert_extern
        | Extern_convert_any | Select _ | I31 _ ) as i ->
        i
      | Struct i -> Struct (rewrite_struct_instruction i)
      | Array i -> Array (rewrite_array_instruction i)
    in
    let rewrite_block_type : Binary.block_type -> Binary.block_type =
     fun (type_id_opt, ft) ->
      ( Option.map rewrite_type_id type_id_opt
      , ( List.map (fun (id, vt) -> (id, rewrite_val_type vt)) (fst ft)
        , List.map rewrite_val_type (snd ft) ) )
    in
    let rec rewrite_instruction = function
      | Binary.Simple i -> Binary.Simple (rewrite_simple_instruction i)
      | Block (a, b, e) ->
        Block (a, Option.map rewrite_block_type b, rewrite_expression e)
      | Loop (a, b, e) ->
        Loop (a, Option.map rewrite_block_type b, rewrite_expression e)
      | If_else (a, b, e1, e2) ->
        If_else
          ( a
          , Option.map rewrite_block_type b
          , rewrite_expression e1
          , rewrite_expression e2 )
      | Return_call i -> Return_call (get_unsafe i functions_map)
      | Call i -> Call (get_unsafe i functions_map)
      | Call_indirect (i, typ) ->
        Call_indirect (get_unsafe i tables_map, rewrite_block_type typ)
      | Return_call_indirect (i, typ) ->
        Return_call_indirect (get_unsafe i tables_map, rewrite_block_type typ)
      | Br_on_cast (id, rt1, rt2) ->
        Br_on_cast (id, rewrite_ref_type rt1, rewrite_ref_type rt2)
      | Br_on_cast_fail (id, rt1, rt2) ->
        Br_on_cast_fail (id, rewrite_ref_type rt1, rewrite_ref_type rt2)
      | ( Return | Br _ | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _
        (* TODO: It's weird that return_call_ref is not using an indice like call_ref does... *)
        | Return_call_ref _
        (* TODO: check that call_ref is taking a raw type and not a typed index *)
        | Call_ref _ ) as i ->
        i
    and rewrite_expression expr =
      Annotated.map (List.map (Annotated.map rewrite_instruction)) expr
    in
    let rewrite_binary_func (func : Binary.Func.t) : Extern_func.t Kind.func =
      let body = rewrite_expression func.body in
      let type_f = rewrite_block_type func.type_f in
      Kind.Wasm { func with body; type_f }
    in
    let env =
      List.fold_left
        (fun env (address, func) ->
          match (func : Extern_func.t Kind.func) with
          | Kind.Wasm func ->
            let func = rewrite_binary_func func in
            let functions = Allocator.add_manual address func env.functions in
            { env with functions }
          | Kind.Extern _idx -> assert false )
        env functions
    in
    let env =
      List.fold_left
        (fun env (address, global) ->
          let globals = Allocator.add_manual address global env.globals in
          { env with globals } )
        env globals
    in

    let env =
      List.fold_left
        (fun env (address, memory) ->
          let memories = Allocator.add_manual address memory env.memories in
          { env with memories } )
        env memories
    in
    let env =
      List.fold_left
        (fun env (address, data) ->
          let data = Data.of_string data in
          let datas = Allocator.add_manual address data env.datas in
          { env with datas } )
        env datas
    in

    let rewrite_ref : 'value Value.Ref.t -> 'value Value.Ref.t = function
      | Func (Some i) ->
        (* TODO: this should be rewritten to the right index? *)
        Func (Some i)
      | ( Extern _
        | Func None
        | NullExn | NullRef | NullI31 | I31 _ | Array _ | Struct _
        | ExternAsAny _ | AnyAsExtern _ ) as i ->
        i
    in
    let env =
      List.fold_left
        (fun env (address, elem) ->
          let elem = List.map rewrite_ref elem in
          let elem = Elem.init elem in
          let elems = Allocator.add_manual address elem env.elems in
          { env with elems } )
        env elems
    in

    let env =
      List.fold_left
        (fun env (address, table) ->
          (* TODO: missing rewriting here! *)
          let tables = Allocator.add_manual address table env.tables in
          { env with tables } )
        env tables
    in

    let env =
      List.fold_left
        (fun env (address, tag) ->
          (* TODO: missing rewriting here! *)
          let tags = Allocator.add_manual address tag env.tags in
          { env with tags } )
        env tags
    in

    (* check that there is not two exports with the same names *)
    let* () =
      let names = Hashtbl.create 512 in
      let check_one { Binary.Export.name; _ } =
        if Hashtbl.mem names name then Error `Duplicate_export_name
        else begin
          Hashtbl.add names name ();
          Ok ()
        end
      in
      let check exports = array_iter check_one exports in
      let* () = check modul.exports.func in
      let* () = check modul.exports.global in
      let* () = check modul.exports.mem in
      let* () = check modul.exports.table in
      let* () = check modul.exports.tag in
      Ok ()
    in

    let export_array_to_string_map a address_map =
      Array.fold_left
        (fun map { Binary.Export.name; id } ->
          match IntMap.find_opt id address_map with
          | None -> assert false
          | Some addr -> StringMap.add name addr map )
        StringMap.empty a
    in
    let add_exports new_module exports exported_map =
      if StringMap.is_empty exports then exported_map
      else IntMap.add new_module exports exported_map
    in
    let exported_functions =
      add_exports new_module
        (export_array_to_string_map modul.exports.func functions_map)
        env.exported_functions
    in
    let exported_globals =
      add_exports new_module
        (export_array_to_string_map modul.exports.global globals_map)
        env.exported_globals
    in
    let exported_memories =
      add_exports new_module
        (export_array_to_string_map modul.exports.mem memories_map)
        env.exported_memories
    in
    let exported_tables =
      add_exports new_module
        (export_array_to_string_map modul.exports.table tables_map)
        env.exported_tables
    in
    let exported_tags =
      add_exports new_module
        (export_array_to_string_map modul.exports.tag tags_map)
        env.exported_tags
    in
    let last_module = Some new_module in
    let initialization_codes =
      let initialization_code =
        rewrite_expression (Annotated.dummy initialization_code)
      in
      IntMap.add new_module initialization_code.Annotated.raw
        env.initialization_codes
    in

    let env =
      { env with
        initialization_codes
      ; exported_functions
      ; exported_memories
      ; exported_globals
      ; exported_tables
      ; exported_tags
      ; last_module
      }
    in

    let env =
      match name with
      | None -> env
      | Some name ->
        let registered_modules =
          StringMap.add name new_module env.registered_modules
        in
        { env with registered_modules }
    in

    let env =
      match modul.id with
      | None -> env
      | Some id ->
        let raw_names = StringMap.add id new_module env.raw_names in
        { env with raw_names }
    in

    Log.debug (fun m -> m "env is: %a" pp env);
    Ok env

  let get_global ~env id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id env.globals with
    | Some { value = v; _ } -> v
    | None -> assert false

  let set_global ~env id value =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id env.globals with
    | Some { typ; _ } ->
      let global = { value; typ } in
      let globals = Allocator.add_manual id global env.globals in
      { env with globals }
    | None -> assert false

  let get_memory ~env id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id env.memories with
    | Some m -> m
    | None -> assert false

  let set_memory ~env id memory =
    let id = Allocator.unsafe_of_int id in
    let memories = Allocator.add_manual id memory env.memories in
    { env with memories }

  let get_table ~env id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id env.tables with
    | Some m -> m
    | None -> assert false

  let set_table ~env id table =
    let id = Allocator.unsafe_of_int id in
    let tables = Allocator.add_manual id table env.tables in
    { env with tables }

  let get_elem ~env id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id env.elems with
    | Some m -> m
    | None -> assert false

  (* le bonhomme vert! *)
  let set_elem ~env id elem =
    let id = Allocator.unsafe_of_int id in
    let elems = Allocator.add_manual id elem env.elems in
    { env with elems }

  let get_data ~env id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id env.datas with
    | Some m -> m
    | None -> assert false

  let set_data ~env id data =
    let id = Allocator.unsafe_of_int id in
    let datas = Allocator.add_manual id data env.datas in
    { env with datas }

  let get_func ~env id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id env.functions with
    | Some v -> v
    | None -> assert false

  let get_types ~env = env.types

  let get_type_groups ~env = env.type_groups

  let link_extern_module ~env ~name m =
    Log.debug (fun m -> m "linking extern module: %s" name);
    let new_module = get_next_module ~env in
    let+ env, exports =
      list_fold_left
        (fun (env, exports) (name, func) ->
          let func : Extern_func.t Kind.func = Kind.Extern func in
          let functions, addr = Allocator.add func env.functions in
          if StringMap.mem name exports then Error `Duplicate_export_name
          else
            let exports = StringMap.add name addr exports in
            Ok ({ env with functions }, exports) )
        (env, StringMap.empty) m
    in
    let exported_functions =
      IntMap.add new_module exports env.exported_functions
    in
    let last_module = Some new_module in
    let env = { env with exported_functions; last_module } in
    let registered_modules =
      StringMap.add name new_module env.registered_modules
    in
    { env with registered_modules }

  let get_exported_func ~env ~module_name ~func_name =
    let* modul =
      match module_name with
      | None -> get_last_module ~env
      | Some module_name -> (
        match StringMap.find_opt module_name env.raw_names with
        | None -> Error (`Unbound_module module_name)
        | Some modul -> Ok modul )
    in
    let functions =
      match IntMap.find_opt modul env.exported_functions with
      | None -> assert false
      | Some functions -> functions
    in
    let* address =
      match StringMap.find_opt func_name functions with
      | None -> Error (`Unbound_name func_name)
      | Some v -> Ok v
    in
    match Allocator.find_opt address env.functions with
    | Some func -> Ok func
    | None -> assert false

  let get_exported_global ~env ~module_name ~global_name =
    let* modul =
      match module_name with
      | None -> get_last_module ~env
      | Some module_name -> (
        match StringMap.find_opt module_name env.raw_names with
        | None -> Error (`Unbound_module module_name)
        | Some modul -> Ok modul )
    in
    let globals =
      match IntMap.find_opt modul env.exported_globals with
      | None -> assert false
      | Some globals -> globals
    in
    let* address =
      match StringMap.find_opt global_name globals with
      | None -> Error (`Unbound_name global_name)
      | Some v -> Ok v
    in
    match Allocator.find_opt address env.globals with
    | Some global -> Ok global.value
    | None -> assert false

  let get_context ~env = env.context

  let get_modul_from_modid ~env ~modid =
    match StringMap.find_opt modid env.raw_names with
    | None -> Fmt.error_msg "unbound module %s" modid
    | Some v -> Ok v
end

module Concrete = struct
  module Context = struct
    type t = unit

    let empty () = ()
  end

  include
    Make (Context) (Concrete_value) (Constexpr_eval.Concrete) (Concrete_memory)
      (Concrete_table)
      (Concrete_elem)
      (Concrete_extern.Func)
      (Concrete_data)
end

module Symbolic = struct
  module Context = struct
    type t = unit

    let empty () = ()
  end

  include
    Make (Context) (Symbolic_value) (Constexpr_eval.Symbolic) (Symbolic_memory)
      (Symbolic_table)
      (Symbolic_elem)
      (Symbolic_extern.Func)
      (Symbolic_data)
end

module Abstract = struct
  module Context = struct
    include Abstract_domain.Context

    let empty = Abstract_domain.root_context
  end

  include
    Make (Context) (Abstract_value) (Constexpr_eval.Abstract) (Abstract_memory)
      (Abstract_table)
      (Abstract_elem)
      (Abstract_extern.Func)
      (Abstract_data)
end
