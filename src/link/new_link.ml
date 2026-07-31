(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Eval_const = struct
  module Value = Concrete_value
  module Stack = Stack.Make [@inlined hint] (Value)

  let i32_instr stack : Binary.i32_instr -> _ = function
    | Const i -> Stack.push_i32 stack i
    | Add -> Stack.apply_i32_i32_i32 stack Value.I32.add
    | Sub -> Stack.apply_i32_i32_i32 stack Value.I32.sub
    | Mul -> Stack.apply_i32_i32_i32 stack Value.I32.mul
    | _ -> assert false

  let i64_instr stack : Binary.i64_instr -> _ = function
    | Const i -> Stack.push_i64 stack i
    | Add -> Stack.apply_i64_i64_i64 stack Value.I64.add
    | Sub -> Stack.apply_i64_i64_i64 stack Value.I64.sub
    | Mul -> Stack.apply_i64_i64_i64 stack Value.I64.mul
    | _ -> assert false

  let simple_instruction ~get_func ~get_global stack = function
    | Binary.I32 i -> Result.ok (i32_instr stack i)
    | Binary.I64 i -> Result.ok (i64_instr stack i)
    | F32 (Const f) -> Result.ok @@ Stack.push_f32 stack f
    | F64 (Const f) -> Result.ok @@ Stack.push_f64 stack f
    | V128 (Const f) -> Result.ok @@ Stack.push_v128 stack f
    | Ref (Null t) -> Result.ok @@ Stack.push_ref stack (Value.Ref.null t)
    | Ref (Func id) ->
      let* f = get_func id in
      let value = Value.Ref (Func (Some f)) in
      Result.ok @@ Stack.push stack value
    | Global (Get id) ->
      let* g = get_global id in
      Result.ok @@ Stack.push stack g
    | _ -> assert false

  let instr ~get_func ~get_global stack instr =
    match instr.Annotated.raw with
    | Binary.Simple i -> simple_instruction ~get_func ~get_global stack i
    | _ -> assert false

  (* TODO: the modul parameter can probably be removed *)
  let expr ~get_func ~get_global e : Concrete_value.t Result.t =
    let* stack =
      list_fold_left (instr ~get_func ~get_global) Stack.empty e.Annotated.raw
    in
    match stack with
    | [] -> Error (`Type_mismatch "const expr returning zero values")
    | _ :: _ :: _ ->
      Error (`Type_mismatch "const expr returning more than one value")
    | [ result ] -> Ok result
end

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

module Make (M : Runtime_builder_intf.T) :
  Runtime_intf.T
    with type extern_func := M.extern_func
     and type value := M.value
     and type elem := M.elem
     and type data := M.data
     and type table := M.table
     and type memory := M.memory = struct
  type modul = int

  type context = M.context

  (* when evaluating constant expressions, we don't want to deal with value because building them is annoying and differs too much between the various interpreters, yet, the constant expression builders can read globals that could be values, but we use the fact that it can only read constant globals that are always going to be concrete, doing so allows us to have a single concrete implementation of constant evaluation, with the price of having to convert from concrete to {abstract,symbolic} each time we load a constant global, but who cares, we could simply inline them in the future and don't bother *)
  type global_value =
    | Const of Concrete_value.t
    | Var of M.value

  type global =
    { value : global_value
    ; typ : Binary.Global.Type.t
    }

  type t =
    { functions : Kind.func Allocator.t
        (* map from runtime address to runtime functions *)
    ; extern_functions : (M.extern_func * Binary.func_type) Allocator.t
        (* map from runtime address to runtime extern functions *)
    ; globals : global Allocator.t
        (* map from runtime address to runtime globals *)
    ; memories : M.memory Allocator.t
        (* map from runtime address to runtime memories *)
    ; tables : M.table Allocator.t
        (* map from runtime address to runtime tables *)
    ; datas : M.data Allocator.t (* map from runtime address to runtime datas *)
    ; elems : M.elem Allocator.t (* map from runtime address to runtime elems *)
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
    ; last_module : modul option (* last module that was added to the runtime *)
    ; registered_modules : modul StringMap.t
        (* map from registered names to modul *)
    ; context : M.context
    }

  let pp ppf
    { functions
    ; extern_functions
    ; globals
    ; memories
    ; tables
    ; datas
    ; elems
    ; initialization_codes
    ; exported_functions
    ; exported_globals
    ; exported_memories
    ; exported_tables
    ; last_module
    ; registered_modules
    ; context = _
    } =
    let pp_todo ppf _v = Fmt.pf ppf "<TODO>" in
    let pp_global = pp_todo in
    let pp_elem = pp_todo in
    let pp_table = pp_todo in
    let pp_memory = pp_todo in
    let pp_data = pp_todo in
    let pp_modul ppf v = Fmt.pf ppf "%d" v in
    Fmt.pf ppf
      "@[<v>functions: %a@,\
       extern_functions: %a@,\
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
       registered_modules: %a@]"
      (Allocator.pp Kind.pp_func)
      functions
      (Allocator.pp
         (Fmt.pair (fun ppf _v -> Fmt.pf ppf "<extern>") Binary.pp_func_type) )
      extern_functions (Allocator.pp pp_global) globals (Allocator.pp pp_memory)
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
      registered_modules

  let empty =
    let functions = Allocator.empty in
    let extern_functions = Allocator.empty in
    let globals = Allocator.empty in
    let memories = Allocator.empty in
    let tables = Allocator.empty in
    let datas = Allocator.empty in
    let elems = Allocator.empty in
    let initialization_codes = IntMap.empty in
    let exported_functions = IntMap.empty in
    let exported_globals = IntMap.empty in
    let exported_memories = IntMap.empty in
    let exported_tables = IntMap.empty in
    let last_module = None in
    let registered_modules = StringMap.empty in
    let context = M.empty_context () in
    { functions
    ; extern_functions
    ; globals
    ; memories
    ; tables
    ; datas
    ; elems
    ; initialization_codes
    ; exported_functions
    ; exported_globals
    ; exported_memories
    ; exported_tables
    ; last_module
    ; registered_modules
    ; context
    }

  let register_module ~runtime ~modul ~name =
    let registered_modules =
      StringMap.add name modul runtime.registered_modules
    in
    { runtime with registered_modules }

  let get_last_module ~runtime =
    match runtime.last_module with
    | None -> Error (`Unknown_module "there was no last module")
    | Some modul -> Ok modul

  let get_registered_module ~runtime ~name =
    match StringMap.find_opt name runtime.registered_modules with
    | None -> Error (`Unknown_module name)
    | Some modul -> Ok modul

  let get_next_module ~runtime =
    match runtime.last_module with None -> 0 | Some modul -> succ modul

  let get_initialization_code ~runtime ~modul : Binary.expr =
    match IntMap.find_opt modul runtime.initialization_codes with
    | Some expr -> expr
    | None -> []

  let load_exported_key exported ~runtime ~modul_name ~name =
    (* find the source module *)
    let* modul = get_registered_module ~runtime ~name:modul_name in
    (* finc the exports for this module *)
    match IntMap.find_opt modul exported with
    | None ->
      (* it should be there! *)
      assert false
    | Some names ->
      (* find the address for the export with the desired name *)
      begin match StringMap.find_opt name names with
      | None -> Error (`Unknown_import (modul_name, name))
      | Some address -> Ok address
      end

  let load_import ~runtime ~import:({ modul_name; name; _ } : _ Origin.imported)
    exported allocator =
    (* find the address of the map *)
    let* address = load_exported_key exported ~runtime ~modul_name ~name in
    (* find its runtime value *)
    match Allocator.find_opt address allocator with
    | None ->
      (* it should be there! *)
      assert false
    | Some func -> Ok (func, address)

  let link_function ~runtime id (functions, map) = function
    | Origin.Local func ->
      let func : Kind.func = Kind.Wasm func in
      let address =
        Allocator.plus_key (Allocator.next_key runtime.functions) id
      in
      let functions = (address, func) :: functions in
      let map = IntMap.add id address map in
      Ok (functions, map)
    | Imported ({ name; typ; _ } as import) ->
      let* func, address =
        load_import ~runtime ~import runtime.exported_functions
          runtime.functions
      in
      (* comparing their types *)
      let* () =
        let (Binary.Bt_raw (_, typ)) = typ in
        let typ' =
          match (func : Kind.func) with
          | Kind.Wasm func ->
            let (Bt_raw ((None | Some _), t)) = func.type_f in
            t
          | Kind.Extern addr ->
            let addr = Allocator.unsafe_of_int addr in
            let _f, t =
              match Allocator.find_opt addr runtime.extern_functions with
              | None -> assert false
              | Some v -> v
            in
            t
        in
        if Binary.func_type_eq typ typ' then Ok ()
        else
          let msg =
            Fmt.str "%s: expected: %a got: %a" name Binary.pp_func_type typ
              Binary.pp_func_type typ'
          in
          Error (`Incompatible_import_type msg)
      in
      (* adding new global to the address map *)
      let map = IntMap.add id address map in
      Ok (functions, map)

  let link_global ~runtime id (globals, map) = function
    | Origin.Local ({ init; typ; id = _ } : Binary.Global.t) ->
      let* value =
        Eval_const.expr
          ~get_func:(fun _id -> assert false)
          ~get_global:(fun _id -> assert false)
          init
      in
      let value =
        match fst typ with
        | Const -> Const value
        | Var -> Var (M.value_of_concrete runtime.context value)
      in
      let global : global = { value; typ } in

      let address =
        Allocator.plus_key (Allocator.next_key runtime.globals) id
      in
      let globals = (address, global) :: globals in

      let map = IntMap.add id address map in
      Ok (globals, map)
    | Imported ({ name; typ; _ } as import) ->
      let* global, address =
        load_import ~runtime ~import runtime.exported_globals runtime.globals
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

  let link_memory ~runtime id (memories, map) = function
    | Origin.Local (_label, typ) ->
      let memory = M.init_memory typ in

      let address =
        Allocator.plus_key (Allocator.next_key runtime.memories) id
      in
      let memories = (address, memory) :: memories in

      let map = IntMap.add id address map in
      Ok (memories, map)
    | Imported ({ name; typ; _ } as import) ->
      let* memory, address =
        load_import ~runtime ~import runtime.exported_memories runtime.memories
      in
      (* comparing their types *)
      let* () =
        let imported_limit = M.get_memory_limits memory in
        if memory_limit_is_included ~import:typ ~imported:imported_limit () then
          Ok ()
        else Error (`Incompatible_import_type name)
      in
      (* adding new memory to the address map *)
      let map = IntMap.add id address map in
      Ok (memories, map)

  let link_table ~runtime id (tables, map) = function
    | Origin.Local { Binary.Table.id = label; typ; _ } ->
      (* TODO: remove label in the future, it's useless *)
      let table = M.init_table ?label typ in

      let address = Allocator.plus_key (Allocator.next_key runtime.tables) id in
      let tables = (address, table) :: tables in

      let map = IntMap.add id address map in
      Ok (tables, map)
    | Imported ({ name; typ; _ } as import) ->
      let* table, address =
        load_import ~runtime ~import runtime.exported_tables runtime.tables
      in
      (* comparing their types *)
      let* () =
        let imported_data_size = M.get_table_size table in
        let typ' = M.get_table_type table in
        if table_types_are_compatible typ typ' ~imported_data_size then Ok ()
        else Error (`Incompatible_import_type name)
      in
      (* adding new table to the address map *)
      let map = IntMap.add id address map in
      Ok (tables, map)

  let link_data ~runtime ~memories_map id
    ((initialization_code : Binary.expr), datas, map)
    { Binary.Data.init; mode; _ } =
    let data = init in

    let address = Allocator.plus_key (Allocator.next_key runtime.datas) id in
    let datas = (address, data) :: datas in

    let map = IntMap.add id address map in
    let* initialization_code =
      match mode with
      | Passive -> Ok initialization_code
      | Active (mem, offset) ->
        begin match IntMap.find_opt mem memories_map with
        | None -> Error (`Unknown_memory (Text.Raw mem))
        | Some _ ->
          let* offset =
            Eval_const.expr
              ~get_func:(fun _id -> assert false)
              ~get_global:(fun _id -> assert false)
              offset
          in
          let offset =
            match offset with
            | I32 i -> i
            | _ ->
              (* Should have failed earlier at typing *)
              assert false
          in
          let length = String.length init |> Concrete_i32.of_int in
          (* Jean-Christophe, I'm sorry for writing this, please forgive me... *)
          Ok
            ( initialization_code
            @ Annotated.dummies
                [ Binary.Simple (I32 (Const offset))
                ; Simple (I32 (Const 0l))
                ; Simple (I32 (Const length))
                ; Simple (Memory (Init (mem, id)))
                ; Simple (Data (Drop id))
                ] )
        end
    in
    Ok (initialization_code, datas, map)

  let link_elem ~runtime id (initialization_code, elems, map)
    { Binary.Elem.init; mode; _ } =
    let* init =
      list_map
        (Eval_const.expr
           ~get_func:(fun _id -> assert false)
           ~get_global:(fun _id -> assert false) )
        init
    in
    let* elem =
      match mode with
      | Declarative -> (* Declarative elements have no runtime value *) Ok []
      | Active _ | Passive ->
        list_map
          (function
            | Concrete_value.Ref v -> Ok v
            | _ -> Error `Constant_expression_required )
          init
    in
    let elem = M.elem_of_concrete_ref_list elem in

    let address = Allocator.plus_key (Allocator.next_key runtime.elems) id in
    let elems = (address, elem) :: elems in

    let map = IntMap.add id address map in
    match mode with
    | Passive | Declarative -> Ok (initialization_code, elems, map)
    | Active (None, _) ->
      (* TODO: the type in binary should be changed if the None case is eliminated when going from Text to Binary. *)
      assert false
    | Active (Some table, offset) ->
      let length = Int32.of_int @@ List.length init in
      let* offset =
        Eval_const.expr
          ~get_func:(fun _id -> assert false)
          ~get_global:(fun _id -> assert false)
          offset
      in
      let offset = match offset with I32 i -> i | _ -> assert false in
      let initialization_code =
        initialization_code
        @ Annotated.dummies
            [ Binary.Simple (I32 (Const offset))
            ; Simple (I32 (Const 0l))
            ; Simple (I32 (Const length))
            ; Simple (Table (Init (table, id)))
            ; Simple (Elem (Drop id))
            ]
      in
      Ok (initialization_code, elems, map)

  let link_binary_module ~(runtime : t) ~name ~(modul : Binary.Module.t) :
    t Result.t =
    Log.debug (fun m ->
      m "linking binary module: %a" (Fmt.option Fmt.string) name );
    (* This is the first step where we simply allocate the runtime values for functions, globals, memories etc.
             Each one is given a unique address in a global space, and we maintain a map from (module id, {func,global,...} id) to runtime address. *)
    let new_module = get_next_module ~runtime in
    (* functions *)
    let* functions, functions_map =
      array_fold_lefti (link_function ~runtime) ([], IntMap.empty) modul.func
    in
    (* tags *)
    (* TODO *)
    (* globals *)
    let* globals, globals_map =
      array_fold_lefti (link_global ~runtime) ([], IntMap.empty) modul.global
    in
    (* memories *)
    let* memories, memories_map =
      array_fold_lefti (link_memory ~runtime) ([], IntMap.empty) modul.mem
    in
    (* tables *)
    let* tables, tables_map =
      array_fold_lefti (link_table ~runtime) ([], IntMap.empty) modul.table
    in
    (* initialization code *)
    (* 1. data *)
    let* initialization_code, datas, datas_map =
      array_fold_lefti
        (link_data ~runtime ~memories_map)
        ([], [], IntMap.empty) modul.data
    in
    (* 2. elem *)
    let* initialization_code, elems, elems_map =
      array_fold_lefti (link_elem ~runtime)
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

    (* Now this is the second step, where we rewrite all access to use runtime address.
       For instance, if a function contains the instruction global.get 0, the 0 is local to the modul in which the function is defined.
       We look what is the runtime address of this global in the map, by looking the global map at (module_id, 0).
       If the runtime address is say, 42, we rewrite the instruction to be global.get 42. *)
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
    let rewrite_simple_instruction :
      Binary.simple_instruction -> Binary.simple_instruction = function
      | Global i -> Global (rewrite_global_instruction i)
      | I32 i -> I32 (rewrite_i32_instruction i)
      | I64 i -> I64 (rewrite_i64_instruction i)
      | F32 i -> F32 (rewrite_f32_instruction i)
      | F64 i -> F64 (rewrite_f64_instruction i)
      | V128 _ -> assert false
      | I8x16 _ -> assert false
      | I16x8 _ -> assert false
      | I32x4 _ -> assert false
      | I64x2 _ -> assert false
      | F32x4 _ -> assert false
      | F64x2 _ -> assert false
      | Ref _ | Table _ | Elem _ | Memory _ | Data _ -> assert false
      | ( Nop | Local _ | Drop | Unreachable | Any_convert_extern
        | Extern_convert_any | Select _ ) as i ->
        i
      | I31 _ | Struct _ | Array _ -> assert false
    in
    let rec rewrite_instruction = function
      | Binary.Simple i -> Binary.Simple (rewrite_simple_instruction i)
      | Block (a, b, e) -> Block (a, b, rewrite_expression e)
      | Loop (a, b, e) -> Loop (a, b, rewrite_expression e)
      | If_else (a, b, e1, e2) ->
        If_else (a, b, rewrite_expression e1, rewrite_expression e2)
      | Return_call _ -> assert false
      | Return_call_indirect (_, Bt_raw (_, (_, _))) -> assert false
      | Return_call_ref (Bt_raw (_, (_, _))) -> assert false
      | Call i -> Call (get_unsafe i functions_map)
      | Call_indirect (_, Bt_raw (_, (_, _))) -> assert false
      | Call_ref _ -> assert false
      | ( Return | Br _ | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _
        | Br_on_cast _ | Br_on_cast_fail _ ) as i ->
        i
    and rewrite_expression expr =
      Annotated.map (List.map (Annotated.map rewrite_instruction)) expr
    in
    let runtime =
      List.fold_left
        (fun runtime (address, func) ->
          match (func : Kind.func) with
          | Kind.Wasm func ->
            let body = rewrite_expression func.body in
            let func : Kind.func = Kind.Wasm { func with body } in
            let functions =
              Allocator.add_manual address func runtime.functions
            in
            { runtime with functions }
          | Kind.Extern _idx -> assert false )
        runtime functions
    in
    let runtime =
      List.fold_left
        (fun runtime (address, global) ->
          let globals = Allocator.add_manual address global runtime.globals in
          { runtime with globals } )
        runtime globals
    in

    (* TODO! *)
    let _ =
      ( datas
      , datas_map
      , elems
      , elems_map
      , tables
      , tables_map
      , memories
      , memories_map )
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
        runtime.exported_functions
    in
    let exported_globals =
      add_exports new_module
        (export_array_to_string_map modul.exports.global globals_map)
        runtime.exported_globals
    in
    let exported_memories =
      add_exports new_module
        (export_array_to_string_map modul.exports.mem memories_map)
        runtime.exported_memories
    in
    let exported_tables =
      add_exports new_module
        (export_array_to_string_map modul.exports.table tables_map)
        runtime.exported_tables
    in
    let last_module = Some new_module in
    let initialization_codes =
      let initialization_code =
        rewrite_expression (Annotated.dummy initialization_code)
      in
      IntMap.add new_module initialization_code.Annotated.raw
        runtime.initialization_codes
    in

    let runtime =
      { runtime with
        initialization_codes
      ; exported_functions
      ; exported_memories
      ; exported_globals
      ; exported_tables
      ; last_module
      }
    in

    let runtime =
      match name with
      | None -> runtime
      | Some name ->
        let registered_modules =
          StringMap.add name new_module runtime.registered_modules
        in
        { runtime with registered_modules }
    in
    Log.debug (fun m -> m "runtime is: %a" pp runtime);
    Ok runtime

  let get_global ~runtime id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.globals with
    | Some { value = Var v; _ } -> v
    | Some { value = Const v; _ } -> M.value_of_concrete runtime.context v
    | None -> assert false

  let set_global ~runtime id v =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.globals with
    | Some { typ; _ } ->
      let value = Var v in
      let global = { value; typ } in
      let globals = Allocator.add_manual id global runtime.globals in
      { runtime with globals }
    | None -> assert false

  let get_memory ~runtime id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.memories with
    | Some m -> m
    | None -> assert false

  let set_memory ~runtime id memory =
    let id = Allocator.unsafe_of_int id in
    let memories = Allocator.add_manual id memory runtime.memories in
    { runtime with memories }

  let get_table ~runtime id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.tables with
    | Some m -> m
    | None -> assert false

  let set_table ~runtime id table =
    let id = Allocator.unsafe_of_int id in
    let tables = Allocator.add_manual id table runtime.tables in
    { runtime with tables }

  let get_elem ~runtime id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.elems with
    | Some m -> m
    | None -> assert false

  (* le bonhomme vert! *)
  let set_elem ~runtime id elem =
    let id = Allocator.unsafe_of_int id in
    let elems = Allocator.add_manual id elem runtime.elems in
    { runtime with elems }

  let get_data ~runtime id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.datas with
    | Some m -> m
    | None -> assert false

  let set_data ~runtime id data =
    let id = Allocator.unsafe_of_int id in
    let datas = Allocator.add_manual id data runtime.datas in
    { runtime with datas }

  let get_func ~runtime id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.functions with
    | Some v -> v
    | None -> assert false

  let get_extern_func ~runtime id =
    let id = Allocator.unsafe_of_int id in
    match Allocator.find_opt id runtime.extern_functions with
    | Some (f, _typ) -> f
    | None -> assert false

  let link_extern_module ~runtime ~name m =
    Log.debug (fun m -> m "linking extern module: %s" name);
    let new_module = get_next_module ~runtime in
    let runtime, exports =
      List.fold_left
        (fun (runtime, exports) (name, func) ->
          let typ = M.to_func_type func in
          let extern_functions, addr =
            Allocator.add (func, typ) runtime.extern_functions
          in
          let functions, addr =
            Allocator.add
              (Kind.Extern (Allocator.unsafe_to_int addr) : Kind.func)
              runtime.functions
          in
          let exports = StringMap.add name addr exports in
          ({ runtime with extern_functions; functions }, exports) )
        (runtime, StringMap.empty) m
    in
    let exported_functions =
      IntMap.add new_module exports runtime.exported_functions
    in
    let last_module = Some new_module in
    let runtime = { runtime with exported_functions; last_module } in
    register_module ~runtime ~modul:new_module ~name

  let get_exported_func ~runtime ~module_name ~func_name =
    let* modul =
      match module_name with
      | None -> get_last_module ~runtime
      | Some module_name -> (
        match StringMap.find_opt module_name runtime.registered_modules with
        | None -> Error (`Unbound_module module_name)
        | Some modul -> Ok modul )
    in
    let functions =
      match IntMap.find_opt modul runtime.exported_functions with
      | None -> assert false
      | Some functions -> functions
    in
    let* address =
      match StringMap.find_opt func_name functions with
      | None -> Error (`Unbound_name func_name)
      | Some v -> Ok v
    in
    match Allocator.find_opt address runtime.functions with
    | Some func -> Ok func
    | None -> assert false

  let get_exported_global ~runtime ~module_name ~global_name =
    let* modul =
      match module_name with
      | None -> get_last_module ~runtime
      | Some module_name -> (
        match StringMap.find_opt module_name runtime.registered_modules with
        | None -> Error (`Unbound_module module_name)
        | Some modul -> Ok modul )
    in
    let globals =
      match IntMap.find_opt modul runtime.exported_globals with
      | None -> assert false
      | Some globals -> globals
    in
    let* address =
      match StringMap.find_opt global_name globals with
      | None -> Error (`Unbound_name global_name)
      | Some v -> Ok v
    in
    match Allocator.find_opt address runtime.globals with
    | Some global ->
      begin match global.value with
      | Var v -> Ok v
      | Const v -> Ok (M.value_of_concrete runtime.context v)
      end
    | None -> assert false

  let get_context ~runtime = runtime.context
end
