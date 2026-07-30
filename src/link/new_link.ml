(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

[@@@warning "-27"]

[@@@warning "-69"]

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

  let simple_instruction ~modul ~get_func ~get_global stack = function
    | Binary.I32 i -> Result.ok (i32_instr stack i)
    | Binary.I64 i -> Result.ok (i64_instr stack i)
    | F32 (Const f) -> Result.ok @@ Stack.push_f32 stack f
    | F64 (Const f) -> Result.ok @@ Stack.push_f64 stack f
    | V128 (Const f) -> Result.ok @@ Stack.push_v128 stack f
    | Ref (Null t) -> Result.ok @@ Stack.push_ref stack (Value.Ref.null t)
    | Ref (Func id) ->
      let* f = get_func ~modul id in
      let value = Value.Ref (Func (Some f)) in
      Result.ok @@ Stack.push stack value
    | Global (Get id) ->
      let* g = get_global ~modul id in
      Result.ok @@ Stack.push stack g
    | _ -> assert false

  let instr ~modul ~get_func ~get_global stack instr =
    match instr.Annotated.raw with
    | Binary.Simple i -> simple_instruction ~modul ~get_func ~get_global stack i
    | _ -> assert false

  (* TODO: the modul parameter can probably be removed *)
  let expr ~modul ~get_func ~get_global e : Concrete_value.t Result.t =
    let* stack =
      list_fold_left
        (instr ~modul ~get_func ~get_global)
        Stack.empty e.Annotated.raw
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
end = struct
  include Map.Make (Int)

  let add v map =
    let key = cardinal map in
    let map = add key v map in
    (map, key)
end

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

module type Runtime_intf = sig
  type t

  type modul

  type value

  val empty : t

  val get_last_module : runtime:t -> modul Result.t

  val register_module : runtime:t -> modul:modul -> name:string -> t

  val get_initialization_code : runtime:t -> modul:modul -> Binary.expr

  val link_binary_module : runtime:t -> modul:Binary.Module.t -> t Result.t
end

module type Runtime_builder_intf = sig
  type value

  val value_of_concrete : Concrete_value.t -> value

  type memory

  val init_memory : Binary.Mem.Type.limits -> memory

  val get_memory_limits : memory -> Binary.Mem.Type.limits

  type table

  val init_table : ?label:string -> Binary.Table.Type.t -> table

  val get_table_size : table -> int

  (* TODO: could be stored at link time instead *)
  val get_table_type : table -> Binary.Table.Type.t

  type elem

  val elem_of_concrete_ref_list : Concrete_ref.t list -> elem
end

module Make (M : Runtime_builder_intf) :
  Runtime_intf with type value = M.value = struct
  type value = M.value

  type memory = M.memory

  type table = M.table

  type elem = M.elem

  type modul = int

  module SourceId = struct
    type t =
      { modul : modul
      ; id : int
      }

    let compare x1 x2 =
      let modul = compare x1.modul x2.modul in
      if modul = 0 then compare x1.id x2.id else modul
  end

  module AddressMap = Map.Make (SourceId)

  (* when evaluating constant expressions, we don't want to deal with value because building them is annoying and differs too much between the various interpreters, yet, the constant expression builders can read globals that could be values, but we use the fact that it can only read constant globals that are always going to be concrete, doing so allows us to have a single concrete implementation of constant evaluation, with the price of having to convert from concrete to {abstract,symbolic} each time we load a constant global, but who cares, we could simply inline them in the future and don't bother *)
  type global_value =
    | Const of Concrete_value.t
    | Var of value

  type global =
    { value : global_value
    ; typ : Binary.Global.Type.t
    }

  type t =
    { functions : Kind.func Allocator.t
        (* map from runtime address to runtime functions *)
    ; functions_map : Allocator.key AddressMap.t
        (* map from function source id to runtime address *)
    ; globals : global Allocator.t
        (* map from runtime address to runtime globals *)
    ; globals_map : Allocator.key AddressMap.t
        (* map from global source id to runtime address *)
    ; memories : memory Allocator.t
        (* map from runtime address to runtime memories *)
    ; memories_map : Allocator.key AddressMap.t
        (* map from memory source id to runtime address *)
    ; tables : table Allocator.t
        (* map from runtime address to runtime tables *)
    ; tables_map : Allocator.key AddressMap.t
        (* map from table source id to runtime address *)
    ; datas : string Allocator.t (* map from runtime address to runtime datas *)
    ; datas_map : Allocator.key AddressMap.t
        (* map from data source id to runtime address *)
    ; elems : elem Allocator.t (* map from runtime address to runtime elems *)
    ; elems_map : Allocator.key AddressMap.t
        (* map from elem source id to runtime address *)
    ; initialization_codes : Binary.expr IntMap.t
        (* map from modul to their initialization code *)
    ; exports : Binary.Module.Exports.t IntMap.t
        (* map from modul to their declared exports *)
    ; last_module : modul option (* last module that was added to the runtime *)
    ; registered_modules : modul StringMap.t
        (* map from registered names to modul *)
    }

  let empty =
    let functions = Allocator.empty in
    let functions_map = AddressMap.empty in
    let globals = Allocator.empty in
    let globals_map = AddressMap.empty in
    let memories = Allocator.empty in
    let memories_map = AddressMap.empty in
    let tables = Allocator.empty in
    let tables_map = AddressMap.empty in
    let datas = Allocator.empty in
    let datas_map = AddressMap.empty in
    let elems = Allocator.empty in
    let elems_map = AddressMap.empty in
    let initialization_codes = IntMap.empty in
    let exports = IntMap.empty in
    let last_module = None in
    let registered_modules = StringMap.empty in
    { functions
    ; functions_map
    ; globals
    ; globals_map
    ; memories
    ; memories_map
    ; tables
    ; tables_map
    ; datas
    ; datas_map
    ; elems
    ; elems_map
    ; initialization_codes
    ; exports
    ; last_module
    ; registered_modules
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

  let load_import ~(runtime : t)
    ~import:({ modul_name; name; _ } : _ Origin.imported) address_map allocator
      =
    (* find the source module *)
    let* modul = get_registered_module ~runtime ~name:modul_name in
    (* find its local id corresponding to the name we want in its exports *)
    let* id =
      match IntMap.find_opt modul runtime.exports with
      | None ->
        (* it should be there! *)
        assert false
      | Some { func; _ } ->
        begin match
          Array.find_opt
            (fun (export : Binary.Export.t) -> String.equal name export.name)
            func
        with
        | None -> Error (`Unknown_import (modul_name, name))
        | Some { id; _ } -> Ok id
        end
    in
    (* find its address in the functions_map *)
    let address =
      let source : SourceId.t = { modul; id } in
      match AddressMap.find_opt source address_map with
      | None ->
        (* it should be there! *)
        assert false
      | Some address -> address
    in
    (* find its runtime value *)
    match Allocator.find_opt address allocator with
    | None ->
      (* it should be there! *)
      assert false
    | Some func -> Ok (func, address)

  let link_function ~modul id runtime = function
    | Origin.Local func ->
      let func : Kind.func = Kind.wasm ~modul func in
      let functions, address = Allocator.add func runtime.functions in
      let functions_map =
        AddressMap.add { id; modul } address runtime.functions_map
      in
      Ok { runtime with functions; functions_map }
    | Imported ({ name; typ; _ } as import) ->
      let* func, address =
        load_import ~runtime ~import runtime.functions_map runtime.functions
      in
      (* comparing their types *)
      let* () =
        let (Binary.Bt_raw (_, typ)) = typ in
        let typ' =
          match (func : Kind.func) with
          | Kind.Wasm { func; _ } ->
            let (Bt_raw ((None | Some _), t)) = func.type_f in
            t
          | Kind.Extern { idx } -> assert false
          (*
                let _f, t = Dynarray.get ls.extern_modules idx in
                t
                *)
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
      let functions_map =
        AddressMap.add { id; modul } address runtime.functions_map
      in
      Ok { runtime with functions_map }

  let link_global ~modul id runtime = function
    | Origin.Local ({ init; typ; id = _ } : Binary.Global.t) ->
      let* value =
        Eval_const.expr ~modul
          ~get_func:(fun ~modul:_ id -> assert false)
          ~get_global:(fun ~modul:_ id -> assert false)
          init
      in
      let value =
        match fst typ with
        | Const -> Const value
        | Var -> Var (M.value_of_concrete value)
      in
      let global : global = { value; typ } in
      let globals, address = Allocator.add global runtime.globals in
      let globals_map =
        AddressMap.add { id; modul } address runtime.globals_map
      in
      Ok { runtime with globals_map; globals }
    | Imported ({ name; typ; _ } as import) ->
      let* global, address =
        load_import ~runtime ~import runtime.globals_map runtime.globals
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
      let globals_map =
        AddressMap.add { id; modul } address runtime.globals_map
      in
      Ok { runtime with globals_map }

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

  let link_memory ~modul id runtime = function
    | Origin.Local (_label, typ) ->
      let memory = M.init_memory typ in
      let memories, address = Allocator.add memory runtime.memories in
      let memories_map =
        AddressMap.add { id; modul } address runtime.memories_map
      in
      Ok { runtime with memories_map; memories }
    | Imported ({ name; typ; _ } as import) ->
      let* memory, address =
        load_import ~runtime ~import runtime.memories_map runtime.memories
      in
      (* comparing their types *)
      let* () =
        let imported_limit = M.get_memory_limits memory in
        if memory_limit_is_included ~import:typ ~imported:imported_limit () then
          Ok ()
        else Error (`Incompatible_import_type name)
      in
      (* adding new memory to the address map *)
      let memories_map =
        AddressMap.add { id; modul } address runtime.memories_map
      in
      Ok { runtime with memories_map }

  let link_table ~modul id runtime = function
    | Origin.Local { Binary.Table.id = label; typ; _ } ->
      (* TODO: remove label in the future, it's useless *)
      let table = M.init_table ?label typ in
      let tables, address = Allocator.add table runtime.tables in
      let tables_map =
        AddressMap.add { id; modul } address runtime.tables_map
      in
      Ok { runtime with tables_map; tables }
    | Imported ({ name; typ; _ } as import) ->
      let* table, address =
        load_import ~runtime ~import runtime.tables_map runtime.tables
      in
      (* comparing their types *)
      let* () =
        let imported_data_size = M.get_table_size table in
        let typ' = M.get_table_type table in
        if table_types_are_compatible typ typ' ~imported_data_size then Ok ()
        else Error (`Incompatible_import_type name)
      in
      (* adding new table to the address map *)
      let tables_map =
        AddressMap.add { id; modul } address runtime.tables_map
      in
      Ok { runtime with tables_map }

  let link_data ~modul id (runtime, (initialization_code : Binary.expr))
    { Binary.Data.init; mode; _ } =
    let data = init in
    let datas, address = Allocator.add data runtime.datas in
    let datas_map = AddressMap.add { id; modul } address runtime.datas_map in
    let* expr =
      match mode with
      | Passive -> Ok initialization_code
      | Active (mem, offset) ->
        begin match
          AddressMap.find_opt { id = mem; modul } runtime.memories_map
        with
        | None -> Error (`Unknown_memory (Text.Raw mem))
        | Some _ ->
          let* offset =
            Eval_const.expr ~modul
              ~get_func:(fun ~modul:_ _id -> assert false)
              ~get_global:(fun ~modul:_ _id -> assert false)
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
    Ok ({ runtime with datas_map; datas }, expr)

  let link_elem ~modul id (runtime, initialization_code)
    { Binary.Elem.init; mode; _ } =
    let* init =
      list_map
        (Eval_const.expr ~modul
           ~get_func:(fun ~modul:_ _id -> assert false)
           ~get_global:(fun ~modul:_ _id -> assert false) )
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
    let elems, address = Allocator.add elem runtime.elems in
    let elems_map = AddressMap.add { id; modul } address runtime.elems_map in
    let runtime = { runtime with elems_map; elems } in
    match mode with
    | Passive | Declarative -> Ok (runtime, initialization_code)
    | Active (None, _) ->
      (* TODO: the type in binary should be changed if the None case is eliminated when going from Text to Binary. *)
      assert false
    | Active (Some table, offset) ->
      let length = Int32.of_int @@ List.length init in
      let* offset =
        Eval_const.expr ~modul
          ~get_func:(fun ~modul:_ _id -> assert false)
          ~get_global:(fun ~modul:_ _id -> assert false)
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
      Ok (runtime, initialization_code)

  let link_binary_module ~(runtime : t) ~(modul : Binary.Module.t) : t Result.t
      =
    (* This is the first step where we simply allocate the runtime values for functions, globals, memories etc.
       Each one is given a unique address in a global space, and we maintain a map from (module id, {func,global,...} id) to runtime address. *)
    let new_module = get_next_module ~runtime in
    (* functions *)
    let* runtime =
      array_fold_lefti (link_function ~modul:new_module) runtime modul.func
    in
    (* tags *)
    (* TODO *)
    (* globals *)
    let* runtime =
      array_fold_lefti (link_global ~modul:new_module) runtime modul.global
    in
    (* memories *)
    let* runtime =
      array_fold_lefti (link_memory ~modul:new_module) runtime modul.mem
    in
    (* tables *)
    let* runtime =
      array_fold_lefti (link_table ~modul:new_module) runtime modul.table
    in
    (* initialization code *)
    (* 1. data *)
    let* runtime, initialization_code =
      array_fold_lefti (link_data ~modul:new_module) (runtime, []) modul.data
    in
    (* 2. elem *)
    let* runtime, initialization_code =
      array_fold_lefti
        (link_elem ~modul:new_module)
        (runtime, initialization_code)
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

    let exports = IntMap.add new_module modul.exports runtime.exports in
    let last_module = Some new_module in
    let initialization_codes =
      IntMap.add new_module initialization_code runtime.initialization_codes
    in

    (* Now this is the second step, where we rewrite all access to use runtime address.
      For instance, if a function contains the instruction global.get 0, the 0 is local to the modul in which the function is defined.
      We look what is the runtime address of this global in the map, by looking the global map at (module_id, 0).
      If the runtime address is say, 42, we rewrite the instruction to be global.get 42. *)
    let runtime = { runtime with initialization_codes; exports; last_module } in
    Ok runtime
end

module Concrete_runtime_builder : Runtime_builder_intf = struct
  type value = Concrete_value.t

  let value_of_concrete v = v

  type memory = Concrete_memory.t

  let init_memory = Concrete_memory.init

  let get_memory_limits = Concrete_memory.get_limits

  type table = Concrete_table.t

  let init_table = Concrete_table.init

  let get_table_size = Concrete_table.size

  let get_table_type = Concrete_table.get_type

  type elem = Concrete_elem.t

  let elem_of_concrete_ref_list l = { Concrete_elem.value = Array.of_list l }
end

module Symbolic_runtime_builder : Runtime_builder_intf = struct
  type value = Symbolic_value.t

  let value_of_concrete v = Symbolic_value.of_concrete v

  type memory = Symbolic_memory.t

  let init_memory _ = assert false

  let get_memory_limits _ = assert false

  type table = Symbolic_table.t

  let init_table ?label:_ = assert false

  let get_table_size _ = assert false

  let get_table_type _ = assert false

  type elem = Symbolic_elem.t

  let elem_of_concrete_ref_list _ = assert false
end

module Abstract_runtime_builder : Runtime_builder_intf = struct
  type value = Abstract_value.t

  let value_of_concrete _ = assert false

  type memory = Abstract_memory.t

  let init_memory _ = assert false

  let get_memory_limits _ = assert false

  type table = |

  let init_table ?label:_ = assert false

  let get_table_size _ = assert false

  let get_table_type _ = assert false

  type elem = |

  let elem_of_concrete_ref_list _ = assert false
end

module Interpret (Runtime : Runtime_intf) = struct
  let run_simple_instruction ~runtime : Binary.simple_instruction -> Runtime.t =
    function
    | I32 (Const i) ->
      Log.info (fun m -> m "i32.const %ld" i);
      runtime
    | _ -> assert false

  let run_instr ~runtime : Binary.instr Annotated.t -> Runtime.t =
   fun i ->
    match i.Annotated.raw with
    | Simple i -> run_simple_instruction ~runtime i
    | _ -> assert false

  let run_expr ~runtime expr =
    List.fold_left (fun runtime instr -> run_instr ~runtime instr) runtime expr

  let exported_func ~(runtime : Runtime.t) ~(modul : Runtime.modul)
    ~(func : string) : Runtime.t =
    let _ = modul in
    runtime

  let initialization_code ~(runtime : Runtime.t) ~(modul : Runtime.modul) :
    Runtime.t =
    let expr = Runtime.get_initialization_code ~runtime ~modul in
    run_expr ~runtime expr
end

module Concrete_runtime : Runtime_intf = Make (Concrete_runtime_builder)

module Symbolic_runtime : Runtime_intf = Make (Symbolic_runtime_builder)

module Abstract_runtime : Runtime_intf = Make (Abstract_runtime_builder)

module Test (Runtime : Runtime_intf) = struct
  module Interpret = Interpret (Runtime)

  let outcome () =
    let* modul =
      Compile.File.until_validate ~unsafe:false (Fpath.v "new_link.wat")
    in
    let runtime = Runtime.empty in
    let* runtime = Runtime.link_binary_module ~runtime ~modul in
    let* modul = Runtime.get_last_module ~runtime in
    let runtime = Interpret.initialization_code ~runtime ~modul in
    let _runtime = Interpret.exported_func ~runtime ~modul ~func:"f" in
    Ok ()

  let run () =
    match outcome () with
    | Error e ->
      let msg = Result.err_to_string e in
      Log.err (fun m ->
        m "******************************************************** %s" msg )
    | Ok () ->
      Log.info (fun m ->
        m
          "******************************************************** new link \
           OK!" );
      ()
end

module Test_concrete = Test (Concrete_runtime)
module Test_symbolic = Test (Symbolic_runtime)
module Test_abstract = Test (Abstract_runtime)

let run () = ()

(*
  Test_concrete.run ();
  Test_symbolic.run ();
  Test_abstract.run ()
  *)
