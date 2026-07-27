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

  let simple_instruction ~get_func ~get_global stack = function
    | Binary.I32 i -> Result.ok (i32_instr stack i)
    | Binary.I64 i -> Result.ok (i64_instr stack i)
    | F32 (Const f) -> Result.ok @@ Stack.push_f32 stack f
    | F64 (Const f) -> Result.ok @@ Stack.push_f64 stack f
    | V128 (Const f) -> Result.ok @@ Stack.push_v128 stack f
    | Ref (Null t) -> Result.ok @@ Stack.push_ref stack (Value.Ref.null t)
    | Ref (Func f) ->
      let* f = get_func f in
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

  (* TODO: binary+const expr *)
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

  type memory

  type value

  val empty : t

  val get_last_module : runtime:t -> modul Result.t

  val register_module : runtime:t -> modul:modul -> name:string -> t

  val get_initialization_code : runtime:t -> modul:modul -> Binary.expr

  val link_binary_module : runtime:t -> modul:Binary.Module.t -> t Result.t
end

module type Runtime_builder_intf = sig
  type value

  type memory

  val init_memory : Binary.Mem.Type.limits -> memory

  val get_memory_limits : memory -> Binary.Mem.Type.limits

  val value_of_concrete : Concrete_value.t -> value
end

module Make (M : Runtime_builder_intf) :
  Runtime_intf with type value = M.value and type memory = M.memory = struct
  type value = M.value

  type memory = M.memory

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
        Eval_const.expr
          ~get_func:(fun _ -> assert false)
          ~get_global:(fun _ -> assert false)
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

  let link_binary_module ~(runtime : t) ~(modul : Binary.Module.t) : t Result.t
      =
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
    (* TODO *)
    (* initialization code *)
    (* TODO *)
    (* 1. data *)
    (* TODO *)
    (* 2. elem *)
    (* TODO *)
    (* 3. start function *)
    (* TODO *)
    let initialization_code =
      match modul.Binary.Module.start with
      | None -> []
      | Some func ->
        (* TODO *)
        let _ = func in
        []
    in

    let exports = IntMap.add new_module modul.exports runtime.exports in
    let last_module = Some new_module in
    let initialization_codes =
      IntMap.add new_module initialization_code runtime.initialization_codes
    in
    let runtime = { runtime with initialization_codes; exports; last_module } in
    Ok runtime
end

module Concrete_runtime_builder : Runtime_builder_intf = struct
  type value = Concrete_value.t

  let value_of_concrete v = v

  type memory = Concrete_memory.t

  let init_memory = Concrete_memory.init

  let get_memory_limits = Concrete_memory.get_limits
end

module Symbolic_runtime_builder : Runtime_builder_intf = struct
  type value = Symbolic_value.t

  let value_of_concrete v = Symbolic_value.of_concrete v

  type memory = Symbolic_memory.t

  let init_memory _ = assert false

  let get_memory_limits _ = assert false
end

module Abstract_runtime_builder : Runtime_builder_intf = struct
  type value = Abstract_value.t

  let value_of_concrete _ = assert false

  type memory = Abstract_memory.t

  let init_memory _ = assert false

  let get_memory_limits _ = assert false
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

  let outcome =
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
    match outcome with
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

let run () =
  Test_concrete.run ();
  Test_symbolic.run ();
  Test_abstract.run ()
