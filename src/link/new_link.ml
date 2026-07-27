(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

[@@@warning "-27"]

open Syntax

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

module type Runtime_builder_intf = sig
  type value
end

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

module Make (M : Runtime_builder_intf) :
  Runtime_intf with type value = M.value = struct
  type value = M.value

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

  type global =
    { value : value
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
    let initialization_codes = IntMap.empty in
    let exports = IntMap.empty in
    let last_module = None in
    let registered_modules = StringMap.empty in
    { functions
    ; functions_map
    ; globals
    ; globals_map
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

  let eval_constant_expression (_e : Binary.expr Annotated.t) : value =
    assert false

  let link_binary_module ~(runtime : t) ~(modul : Binary.Module.t) : t Result.t
      =
    let new_module = get_next_module ~runtime in
    (* functions *)
    let* _i, functions, functions_map =
      array_fold_left
        (fun (id, functions, functions_map) func ->
          match func with
          | Origin.Local func ->
            let func : Kind.func = Kind.wasm ~modul:new_module func in
            let functions, address = Allocator.add func functions in
            let functions_map =
              AddressMap.add { id; modul = new_module } address functions_map
            in
            Ok (succ id, functions, functions_map)
          | Imported { modul_name; name; typ; _ } -> begin
            (* find the source module *)
            let* modul = get_registered_module ~runtime ~name:modul_name in
            (* find its local id corresponding to the name we want in its exports *)
            let* id =
              match IntMap.find_opt modul runtime.exports with
              | None -> assert false
              | Some { func; _ } ->
                begin match
                  Array.find_opt
                    (fun (exported_func : Binary.Export.t) ->
                      String.equal name exported_func.name )
                    func
                with
                | None -> Error (`Unknown_import (modul_name, name))
                | Some { id; _ } -> Ok id
                end
            in
            (* find its address in the functions_map *)
            let address =
              let source : SourceId.t = { modul; id } in
              match AddressMap.find_opt source functions_map with
              | None -> assert false
              | Some address -> address
            in
            (* find its runtime value *)
            let func =
              match Allocator.find_opt address functions with
              | None -> assert false
              | Some func -> func
            in
            (* comparing their types *)
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
            if Binary.func_type_eq typ typ' then
              let functions_map =
                AddressMap.add { id; modul = new_module } address functions_map
              in
              Ok (succ id, functions, functions_map)
            else
              let msg =
                Fmt.str "%s: expected: %a got: %a" name Binary.pp_func_type typ
                  Binary.pp_func_type typ'
              in
              Error (`Incompatible_import_type msg)
            end )
        (0, runtime.functions, runtime.functions_map)
        modul.func
    in
    (* tags *)
    (* globals *)
    let* _i, globals, globals_map =
      array_fold_left
        (fun (id, globals, globals_map) global ->
          match global with
          | Origin.Local ({ init; typ; id = _ } : Binary.Global.t) ->
            let value = eval_constant_expression init in
            let global : global = { value; typ } in
            let globals, address = Allocator.add global globals in
            let globals_map =
              AddressMap.add { id; modul = new_module } address globals_map
            in
            Ok (succ id, globals, globals_map)
          | Imported { modul_name; name; typ; _ } -> begin
            (* find the source module *)
            let* modul = get_registered_module ~runtime ~name:modul_name in
            (* find its local id corresponding to the name we want in its exports *)
            let* id =
              match IntMap.find_opt modul runtime.exports with
              | None -> assert false
              | Some { global; _ } ->
                begin match
                  Array.find_opt
                    (fun (exported_global : Binary.Export.t) ->
                      String.equal name exported_global.name )
                    global
                with
                | None -> Error (`Unknown_import (modul_name, name))
                | Some { id; _ } -> Ok id
                end
            in
            (* find its address in the functions_map *)
            let address =
              let source : SourceId.t = { modul; id } in
              match AddressMap.find_opt source globals_map with
              | None -> assert false
              | Some address -> address
            in
            (* find its runtime value *)
            let global =
              match Allocator.find_opt address globals with
              | None -> assert false
              | Some global -> global
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

            let globals_map =
              AddressMap.add { id; modul = new_module } address globals_map
            in
            Ok (succ id, globals, globals_map)
            end )
        (0, runtime.globals, runtime.globals_map)
        modul.global
    in
    (* memories *)
    (* tables *)
    (* initialization code *)
    (* 1. data *)
    (* 2. elem *)
    (* 3. start function *)
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
    let registered_modules = runtime.registered_modules in
    let runtime =
      { functions
      ; functions_map
      ; globals
      ; globals_map
      ; initialization_codes
      ; exports
      ; last_module
      ; registered_modules
      }
    in
    Ok runtime
end

module Concrete_runtime_builder : Runtime_builder_intf = struct
  type value = Concrete_value.t
end

module Symbolic_runtime_builder : Runtime_builder_intf = struct
  type value = Symbolic_value.t
end

module Abstract_runtime_builder : Runtime_builder_intf = struct
  type value = Abstract_value.t
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
