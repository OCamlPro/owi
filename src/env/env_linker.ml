(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

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

        module Array : Array_intf.T

        module Struct : Struct_intf.T

        type 'value t =
          | Extern of Extern.t option
          | Func of int option
          | NullExn
          | NullRef
          | I31 of i32
          | NullI31
          | Array of 'value Array.t
          | Struct of 'value Struct.t
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
    (Table : Table_intf.T with type reference := Value.t Value.Ref.t)
    (Elem : Elem_intf.T with type reference := Value.t Value.Ref.t)
    (Extern_func : sig
      type t

      val to_func_type : t -> Binary.func_type
    end)
    (Data : Data_intf.T) : sig
  type t =
    ( Extern_func.t
    , Value.t
    , Memory.t
    , Table.t
    , Data.t
    , Elem.t
    , Context.t )
    Env0.t

  val pp : t Fmt.t

  type context = Context.t

  val link_binary_module :
    env:t -> name:string option -> modul:Binary.Module.t -> t Result.t

  val default_gc_val : Binary.storage_type -> Value.t
end = struct
  include Env0

  let default_gc_val = Constexpr_eval.default_gc_val

  type context = Context.t

  type t =
    ( Extern_func.t
    , Value.t
    , Memory.t
    , Table.t
    , Data.t
    , Elem.t
    , Context.t )
    Env0.t

  let pp = Env0.pp ~pp_global:Value.pp ~pp_table:Table.pp

  type link_state =
    { rewrite_map : Env_rewriter.t
    ; functions : (int * Extern_func.t Kind.func) list
    ; globals : (int * Value.t Env0.global) list
    ; memories : (int * Memory.t) list
    ; tables : (int * Table.t) list
    ; datas : (int * string) list
    ; elems : (int * Value.t Value.Ref.t list) list
    ; tags : (int * Binary.Tag.t) list
    ; initialization_code : Binary.expr
    }

  let empty_link_state =
    { rewrite_map = Env_rewriter.empty
    ; functions = []
    ; globals = []
    ; memories = []
    ; tables = []
    ; datas = []
    ; elems = []
    ; tags = []
    ; initialization_code = []
    }

  let link_function ~(env : t) id (link_state : link_state) = function
    | Origin.Local func ->
      let address = Allocator.next_key env.functions + id in
      let functions =
        let func : _ Kind.func = Kind.Wasm func in
        (address, func) :: link_state.functions
      in
      let rewrite_map =
        let functions =
          IntMap.add id address link_state.rewrite_map.functions
        in
        { link_state.rewrite_map with functions }
      in
      Ok { link_state with rewrite_map; functions }
    | Imported ({ name; typ; _ } as import) ->
      let* func, address =
        load_import ~(env : t) ~import env.exported_functions env.functions
      in
      (* comparing their types *)
      let* () =
        let expected_type_id =
          Option.map
            (fun id -> id + link_state.rewrite_map.type_base_id)
            (fst typ)
        in
        let _, expected_ft = typ in
        let got_type_id, got_ft =
          match (func : _ Kind.func) with
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
      let rewrite_map =
        let functions =
          IntMap.add id address link_state.rewrite_map.functions
        in
        { link_state.rewrite_map with functions }
      in
      Ok { link_state with rewrite_map }

  let link_global ctx ~get_const_type ~get_const_global ~(env : t) id link_state
      = function
    | Origin.Local ({ init; typ; id = _ } : Binary.Global.t) ->
      let address = Allocator.next_key env.globals + id in
      let value =
        let e =
          Env_rewriter.rewrite_expression init ~map:link_state.rewrite_map
        in
        Constexpr_eval.expr ctx ~get_const_type ~get_const_global
          e.Annotated.raw
      in
      let globals =
        let global : _ Env0.global = { value; typ } in
        (address, global) :: link_state.globals
      in
      let rewrite_map =
        let globals = IntMap.add id address link_state.rewrite_map.globals in
        { link_state.rewrite_map with globals }
      in
      Ok { link_state with globals; rewrite_map }
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
      let rewrite_map =
        let globals = IntMap.add id address link_state.rewrite_map.globals in
        { link_state.rewrite_map with globals }
      in
      Ok { link_state with rewrite_map }

  let link_memory ~(env : t) id link_state = function
    | Origin.Local (_label, typ) ->
      let address = Allocator.next_key env.memories + id in
      let memories =
        let memory = Memory.init typ in
        (address, memory) :: link_state.memories
      in
      let rewrite_map =
        let memories = IntMap.add id address link_state.rewrite_map.memories in
        { link_state.rewrite_map with memories }
      in
      Ok { link_state with memories; rewrite_map }
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
      let rewrite_map =
        let memories = IntMap.add id address link_state.rewrite_map.memories in
        { link_state.rewrite_map with memories }
      in
      Ok { link_state with rewrite_map }

  let link_table ~(env : t) id link_state = function
    | Origin.Local { Binary.Table.typ; init; _ } ->
      let address = Allocator.next_key env.tables + id in
      let table = Table.init typ in
      let tables = (address, table) :: link_state.tables in
      let rewrite_map =
        let tables = IntMap.add id address link_state.rewrite_map.tables in
        { link_state.rewrite_map with tables }
      in
      let initialization_code =
        match init with
        | None -> link_state.initialization_code
        | Some expr ->
          let len = Int32.of_int (Table.size table) in
          link_state.initialization_code
          @ Annotated.dummies [ Binary.Simple (I32 (Const 0l)) ]
          @ expr.Annotated.raw
          @ Annotated.dummies
              [ Binary.Simple (I32 (Const len)); Simple (Table (Fill id)) ]
      in
      Ok { link_state with tables; rewrite_map; initialization_code }
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
      let rewrite_map =
        let tables = IntMap.add id address link_state.rewrite_map.tables in
        { link_state.rewrite_map with tables }
      in
      Ok { link_state with rewrite_map }

  let link_data ~(env : t) id link_state { Binary.Data.init; mode; _ } =
    let address = Allocator.next_key env.datas + id in
    let datas = (address, init) :: link_state.datas in
    let rewrite_map =
      let datas = IntMap.add id address link_state.rewrite_map.datas in
      { link_state.rewrite_map with datas }
    in
    match mode with
    | Passive -> { link_state with datas; rewrite_map }
    | Active (mem, offset) ->
      let initialization_code =
        (* Jean-Christophe, I'm sorry for writing this, please forgive me... *)
        link_state.initialization_code @ offset.raw
        @ Annotated.dummies
            [ Binary.Simple (I32 (Const 0l))
            ; Simple (I32 (Const (String.length init |> Concrete_i32.of_int)))
            ; Simple (Memory (Init (mem, id)))
            ; Simple (Data (Drop id))
            ]
      in
      { link_state with datas; initialization_code; rewrite_map }

  let link_elem ctx ~get_const_type ~get_const_global ~(env : t) id link_state
    { Binary.Elem.init; mode; _ } =
    let address = Allocator.next_key env.elems + id in
    let rewrite_map =
      let elems = IntMap.add id address link_state.rewrite_map.elems in
      { link_state.rewrite_map with elems }
    in

    let* elems =
      let* elem =
        match mode with
        | Declarative ->
          (* Declarative elements have no runtime value *)
          (* TODO: could we avoid putting anything in the list then? *)
          Ok []
        | Active _ | Passive ->
          let init =
            List.map
              (fun expr ->
                let expr =
                  Env_rewriter.rewrite_expression expr
                    ~map:link_state.rewrite_map
                in
                Constexpr_eval.ref_expr ctx ~get_const_type ~get_const_global
                  expr.Annotated.raw )
              init
          in
          Ok init
      in
      Ok ((address, elem) :: link_state.elems)
    in

    match mode with
    | Passive | Declarative -> Ok { link_state with elems; rewrite_map }
    | Active (table, offset) ->
      let initialization_code =
        link_state.initialization_code @ offset.raw
        @ Annotated.dummies
            [ Binary.Simple (I32 (Const 0l))
            ; Simple (I32 (Const (Int32.of_int @@ List.length init)))
            ; Simple (Table (Init (table, id)))
            ; Simple (Elem (Drop id))
            ]
      in
      Ok { link_state with elems; rewrite_map; initialization_code }

  let link_tag ~(env : t) id (link_state : link_state) = function
    | Origin.Local tag ->
      let address = Allocator.next_key env.tags + id in
      let tags = (address, tag) :: link_state.tags in
      let rewrite_map =
        let tags = IntMap.add id address link_state.rewrite_map.tags in
        { link_state.rewrite_map with tags }
      in
      Ok { link_state with tags; rewrite_map }
    | Imported ({ name; typ; _ } as import) ->
      let* tag, address = load_import ~env ~import env.exported_tags env.tags in
      (* comparing their types *)
      let* () =
        let _, typ = typ in
        let _, actual_type = tag.typ in
        if Binary.func_type_eq typ actual_type then Ok ()
        else
          let msg =
            Fmt.str "%s: expected: %a got: %a" name Binary.pp_func_type typ
              Binary.pp_func_type actual_type
          in
          Error (`Incompatible_import_type msg)
      in
      (* adding new table to the address map *)
      let rewrite_map =
        let tags = IntMap.add id address link_state.rewrite_map.tags in
        { link_state.rewrite_map with tags }
      in
      Ok { link_state with rewrite_map }

  let link_binary_module ~(env : t) ~name ~(modul : Binary.Module.t) :
    t Result.t =
    Log.info (fun m -> m "linking      ...");

    (* This is the first step where we simply allocate the env values for functions, globals, memories etc.
     Each one is given a unique address in a global space, and we maintain a map from (module id, {func,global,...} id) to env address. *)
    let new_module = get_next_module ~env in

    (* type_base_id: the number all previously identified types from the modules
     that were treated before *)
    let type_base_id = Array.length env.types in
    let link_state : link_state =
      let rewrite_map : Env_rewriter.t =
        { Env_rewriter.empty with type_base_id }
      in
      { empty_link_state with rewrite_map }
    in

    let types =
      Array.map
        (Env_rewriter.rewrite_sub_type ~map:link_state.rewrite_map)
        modul.types
    in
    let type_groups =
      let module_groups =
        Binary.compute_type_groups modul.type_defs (Array.length modul.types)
      in
      Array.map
        (fun (lo, size) ->
          (Env_rewriter.rewrite_type_id ~map:link_state.rewrite_map lo, size) )
        module_groups
    in

    let types = Array.append env.types types in
    let type_groups = Array.append env.type_groups type_groups in
    (* We need to compute the updated environment with the new types and type
     groups earlier so that we can use it for imported functions *)

    let env = { env with types; type_groups } in
    (* TODO: should it be passed to the function instead? *)
    let ctx = Context.empty () in
    (* functions *)
    let* link_state =
      array_fold_lefti (link_function ~env) link_state modul.func
    in

    (* tags *)
    (* TODO *)

    (* TODO: I'm not sure about this *)
    let get_const_type id = Array.get types id in

    let get_const_global ~(env : t) globals id =
      (* we should only make visible previously defined immutable globals and imported immutable globals. *)
      begin match List.assoc_opt id globals with
      | Some g -> g.Env0.value
      | None ->
        begin match Allocator.find_opt id env.globals with
        | Some g -> g.value
        | None -> assert false
        end
      end
    in

    (* globals *)
    let* link_state =
      array_fold_lefti
        (fun id link_state ->
          link_global ctx ~get_const_type
            ~get_const_global:(get_const_global ~env link_state.globals)
            ~env id link_state )
        link_state modul.global
    in
    (* memories *)
    let* link_state =
      array_fold_lefti (link_memory ~env) link_state modul.mem
    in
    (* tables *)
    let* link_state =
      array_fold_lefti (link_table ~env) link_state modul.table
    in
    (* tags *)
    (* TODO: rewrite tags later using tags_map, it has not been done for now... *)
    let* link_state = array_fold_lefti (link_tag ~env) link_state modul.tag in

    (* initialization code *)
    (* 1. data *)
    let _n, link_state =
      Array.fold_left
        (fun (i, link_state) data -> (i + 1, link_data ~env i link_state data))
        (0, link_state) modul.data
    in

    (* 2. elem *)
    let* link_state : link_state =
      array_fold_lefti
        (link_elem ctx ~get_const_type
           ~get_const_global:(get_const_global ~env link_state.globals)
           ~env )
        link_state modul.elem
    in

    (* 3. start function *)
    let initialization_code =
      match modul.Binary.Module.start with
      | None -> link_state.initialization_code
      | Some func ->
        link_state.initialization_code @ [ Annotated.dummy (Binary.Call func) ]
    in

    (* Now this is the second step, where we rewrite all access to use env address.
                                   For instance, if a function contains the instruction global.get 0, the 0 is local to the modul in which the function is defined.
                                   We look what is the env address of this global in the map, by looking the global map at (module_id, 0).
                                   If the env address is say, 42, we rewrite the instruction to be global.get 42. *)
    let env =
      List.fold_left
        (fun (env : t) (address, func) ->
          match (func : Extern_func.t Kind.func) with
          | Kind.Wasm func ->
            let func =
              Env_rewriter.rewrite_binary_func ~map:link_state.rewrite_map func
            in
            let functions = Allocator.add_manual address func env.functions in
            { env with functions }
          | Kind.Extern _idx -> assert false )
        env link_state.functions
    in
    let env =
      List.fold_left
        (fun (env : t) (address, global) ->
          let globals = Allocator.add_manual address global env.globals in
          { env with globals } )
        env link_state.globals
    in

    let env =
      List.fold_left
        (fun (env : t) (address, memory) ->
          let memories = Allocator.add_manual address memory env.memories in
          { env with memories } )
        env link_state.memories
    in
    let env =
      List.fold_left
        (fun (env : t) (address, data) ->
          let data = Data.of_string data in
          let datas = Allocator.add_manual address data env.datas in
          { env with datas } )
        env link_state.datas
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
        (fun (env : t) (address, elem) ->
          let elem = List.map rewrite_ref elem in
          let elem = Elem.init elem in
          let elems = Allocator.add_manual address elem env.elems in
          { env with elems } )
        env link_state.elems
    in

    let env =
      List.fold_left
        (fun (env : t) (address, table) ->
          (* TODO: missing rewriting here! *)
          let tables = Allocator.add_manual address table env.tables in
          { env with tables } )
        env link_state.tables
    in

    let env =
      List.fold_left
        (fun (env : t) (address, tag) ->
          (* TODO: missing rewriting here! *)
          let tags = Allocator.add_manual address tag env.tags in
          { env with tags } )
        env link_state.tags
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
        (export_array_to_string_map modul.exports.func
           link_state.rewrite_map.functions )
        env.exported_functions
    in
    let exported_globals =
      add_exports new_module
        (export_array_to_string_map modul.exports.global
           link_state.rewrite_map.globals )
        env.exported_globals
    in
    let exported_memories =
      add_exports new_module
        (export_array_to_string_map modul.exports.mem
           link_state.rewrite_map.memories )
        env.exported_memories
    in
    let exported_tables =
      add_exports new_module
        (export_array_to_string_map modul.exports.table
           link_state.rewrite_map.tables )
        env.exported_tables
    in
    let exported_tags =
      add_exports new_module
        (export_array_to_string_map modul.exports.tag
           link_state.rewrite_map.tags )
        (* TODO: use rewrite_map.tags instead *)
        env.exported_tags
    in
    let last_module = Some new_module in
    let initialization_codes =
      let initialization_code =
        Env_rewriter.rewrite_expression ~map:link_state.rewrite_map
          (Annotated.dummy initialization_code)
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
end
