(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'value global =
  { value : 'value
  ; typ : Binary.Global.Type.t
  }

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

type ('extern_func, 'value, 'memory, 'table, 'data, 'elem, 'context) t =
  { functions : 'extern_func Kind.func Allocator.t
      (* map from runtime address to runtime functions *)
  ; globals : 'value global Allocator.t
      (* map from runtime address to runtime globals *)
  ; memories : 'memory Allocator.t
      (* map from runtime address to runtime memories *)
  ; tables : 'table Allocator.t (* map from runtime address to runtime tables *)
  ; datas : 'data Allocator.t (* map from runtime address to runtime datas *)
  ; elems : 'elem Allocator.t (* map from runtime address to runtime elems *)
  ; tags :
      Binary.Tag.t Allocator.t (* map from runtime address to runtime tags *)
  ; initialization_codes : Binary.expr IntMap.t
      (* map from modul to their initialization code *)
  ; exported_functions : int StringMap.t IntMap.t
      (* map from modul to their exported functions *)
  ; exported_globals : int StringMap.t IntMap.t
      (* map from modul to their exported globals *)
  ; exported_memories : int StringMap.t IntMap.t
      (* map from modul to their exported memories *)
  ; exported_tables : int StringMap.t IntMap.t
      (* map from modul to their exported tables *)
  ; exported_tags : int StringMap.t IntMap.t
  ; last_module : int option (* last module that was added to the runtime *)
  ; registered_modules : int StringMap.t
      (* map from registered names to modul *)
  ; context : 'context
  ; raw_names : int StringMap.t
      (* this is used only for scripts where modules can get a $id and we have to remember them to be able to register them this way... *)
  ; types : Binary.sub_type array
      (* table of all modules types (with the ids shifted) *)
  ; type_groups : (int * int) array
      (* table of all type groups (with their bound ids shifted) *)
  }

let pp ~pp_global ~pp_table ppf
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
  let pp_global ppf v = pp_global ppf v.value in
  let pp_elem = pp_todo in
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
    functions (Allocator.pp pp_global) globals (Allocator.pp pp_memory) memories
    (Allocator.pp pp_table) tables (Allocator.pp pp_data) datas
    (Allocator.pp pp_elem) elems
    (IntMap.pp (fun ppf e ->
       Binary.pp_expr ~short:true ppf (Annotated.dummy e) ) )
    initialization_codes
    (IntMap.pp (StringMap.pp Fmt.int))
    exported_functions
    (IntMap.pp (StringMap.pp Fmt.int))
    exported_globals
    (IntMap.pp (StringMap.pp Fmt.int))
    exported_memories
    (IntMap.pp (StringMap.pp Fmt.int))
    exported_tables (Fmt.option pp_modul) last_module (StringMap.pp pp_modul)
    registered_modules pp_types types pp_type_groups type_groups

let empty ~context =
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

type modul = int

let get_last_module ~env =
  match env.last_module with
  | None -> Error (`Unknown_module "there was no last module")
  | Some modul -> Ok modul

open Syntax

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

let get_global ~env id =
  match Allocator.find_opt id env.globals with
  | Some { value = v; _ } -> v
  | None -> assert false

let set_global ~env id value =
  match Allocator.find_opt id env.globals with
  | Some { typ; _ } ->
    let global = { value; typ } in
    let globals = Allocator.add_manual id global env.globals in
    { env with globals }
  | None -> assert false

let get_memory ~env id =
  match Allocator.find_opt id env.memories with
  | Some m -> m
  | None -> assert false

let set_memory ~env id memory =
  let memories = Allocator.add_manual id memory env.memories in
  { env with memories }

let get_table ~env id =
  match Allocator.find_opt id env.tables with
  | Some m -> m
  | None -> assert false

let set_table ~env id table =
  let tables = Allocator.add_manual id table env.tables in
  { env with tables }

let get_elem ~env id =
  match Allocator.find_opt id env.elems with
  | Some m -> m
  | None -> assert false

(* le bonhomme vert! *)
let set_elem ~env id elem =
  let elems = Allocator.add_manual id elem env.elems in
  { env with elems }

let get_data ~env id =
  match Allocator.find_opt id env.datas with
  | Some m -> m
  | None -> assert false

let set_data ~env id data =
  let datas = Allocator.add_manual id data env.datas in
  { env with datas }

let get_func ~env id =
  match Allocator.find_opt id env.functions with
  | Some v -> v
  | None -> assert false

let get_types ~env = env.types

let get_type_groups ~env = env.type_groups

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

let link_extern_module ~env ~name m =
  Log.debug (fun m -> m "linking extern module: %s" name);
  let new_module = get_next_module ~env in
  let+ env, exports =
    list_fold_left
      (fun ((env : _ t), exports) (name, func) ->
        let func : _ Kind.func = Kind.Extern func in
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
