open Types
open Types.Simplified
open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let page_size = 65_536

module Memory = struct
  type t =
    { id : int
    ; label : string option
    ; mutable limits : limits
    ; mutable data : bytes
    }

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      !r

  let init ?label (typ : limits) : t =
    let data = Bytes.make (page_size * typ.min) '\x00' in
    { id = fresh (); label; limits = typ; data }

  let update_memory mem data =
    let limits =
      { mem.limits with
        min = max mem.limits.min (Bytes.length data / page_size)
      }
    in
    mem.limits <- limits;
    mem.data <- data

  let get_data { data; _ } = data

  let get_limit_max { limits; _ } = limits.max
end

module Table = struct
  (* TODO: Value.ref_value array, gadt to constraint to the right ref_type ? *)
  type 'env table = 'env Value.ref_value array

  type 'env t =
    { id : int
    ; label : string option
    ; limits : limits
    ; typ : ref_type
    ; mutable data : 'env table
    }

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      !r

  let init ?label (typ : table_type) : 'env t =
    let limits, ((_null, heap_type) as ref_type) = typ in
    let null = Value.ref_null' heap_type in
    let table = Array.make limits.min null in
    { id = fresh (); label; limits; typ = ref_type; data = table }

  let update table data = table.data <- data
end

module Global = struct
  type 'env t =
    { mutable value : 'env Value.t
    ; label : string option
    ; mut : mut
    ; typ : val_type
    }
end

(* TODO efficient imap for contiguous index (array) *)
module IMap = Map.Make (Int)

module Env = struct
  type data = { mutable value : string }

  let drop_data data = data.value <- ""

  type 'env elem = { mutable value : 'env Value.ref_value array }

  let drop_elem elem = elem.value <- [||]

  type t =
    { globals : t' Global.t IMap.t
    ; memories : Memory.t IMap.t
    ; tables : t' Table.t IMap.t
    ; functions : t' Value.Func.t IMap.t
    ; data : data IMap.t
    ; elem : t' elem IMap.t
    }

  and t' = t lazy_t

  let pp fmt t =
    let global fmt (id, (global : 'a Global.t)) =
      Format.fprintf fmt "%a -> %a" Format.pp_print_int id Value.pp global.value
    in
    let func fmt (id, (_func : 'a Value.Func.t)) =
      Format.fprintf fmt "%a -> func" Format.pp_print_int id
    in
    Format.fprintf fmt "@[<hov 2>{@ (globals %a)@ (functions %a)@ }@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         global )
      (IMap.bindings t.globals)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         func )
      (IMap.bindings t.functions)

  let empty =
    { globals = IMap.empty
    ; memories = IMap.empty
    ; tables = IMap.empty
    ; functions = IMap.empty
    ; data = IMap.empty
    ; elem = IMap.empty
    }

  let add_global id const env =
    { env with globals = IMap.add id const env.globals }

  let add_memory id mem env =
    { env with memories = IMap.add id mem env.memories }

  let add_table id table env =
    { env with tables = IMap.add id table env.tables }

  let add_func id func env =
    { env with functions = IMap.add id func env.functions }

  let add_data id data env = { env with data = IMap.add id data env.data }

  let add_elem id elem env = { env with elem = IMap.add id elem env.elem }

  let get_global (env : t) id =
    match IMap.find_opt id env.globals with
    | None ->
      Log.debug "%a@." pp env;
      Error "unknown global"
    | Some v -> Ok v

  let get_memory (env : t) id =
    match IMap.find_opt id env.memories with
    | None ->
      Log.debug "%a@." pp env;
      Error "unknown memory"
    | Some v -> Ok v

  let get_table (env : t) id =
    match IMap.find_opt id env.tables with
    | None ->
      Log.debug "%a@." pp env;
      Error "unknown table"
    | Some v -> Ok v

  let get_func (env : t) id =
    match IMap.find_opt id env.functions with
    | None ->
      Log.debug "%a@." pp env;
      error_s "unknown function %a" Format.pp_print_int id
    | Some v -> Ok v

  let get_data (env : t) id =
    match IMap.find_opt id env.data with
    | None ->
      Log.debug "%a@." pp env;
      Error "unknown data"
    | Some v -> Ok v

  let get_elem (env : t) id =
    match IMap.find_opt id env.elem with
    | None ->
      Log.debug "%a@." pp env;
      Error "unknown elem"
    | Some v -> Ok v
end

type global = Env.t' Global.t

type table = Env.t' Table.t

type func = Env.t' Value.Func.t

type exports =
  { globals : Env.t' Global.t StringMap.t
  ; memories : Memory.t StringMap.t
  ; tables : table StringMap.t
  ; functions : func StringMap.t
  ; defined_names : StringSet.t
  }

type module_to_run =
  { modul : modul
  ; env : Env.t
  ; to_run : expr list
  }

type state =
  { by_name : exports StringMap.t
  ; by_id : exports StringMap.t
  ; last : exports option
  }

type extern_module = { functions : (string * Value.Func.extern_func) list }

let empty_state =
  { by_name = StringMap.empty; by_id = StringMap.empty; last = None }

module Const_interp = struct
  type env = Env.t

  let exec_ibinop stack nn (op : Const.ibinop) =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      Stack.push_i32 stack
        (let open Int32 in
         match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2 )
    | S64 ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      Stack.push_i64 stack
        (let open Int64 in
         match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2 )

  let exec_instr (env : env) (stack : Env.t' Stack.t) (instr : Const.instr) =
    match instr with
    | I32_const n -> ok @@ Stack.push_i32 stack n
    | I64_const n -> ok @@ Stack.push_i64 stack n
    | F32_const f -> ok @@ Stack.push_f32 stack f
    | F64_const f -> ok @@ Stack.push_f64 stack f
    | I_binop (nn, op) -> ok @@ exec_ibinop stack nn op
    | Ref_null t -> ok @@ Stack.push stack (Value.ref_null t)
    | Ref_func f ->
      let* f = Env.get_func env f in
      let value = Value.Ref (Funcref (Some f)) in
      ok @@ Stack.push stack value
    | Global_get id ->
      let* g = Env.get_global env id in
      ok @@ Stack.push stack g.value
    | Array_new_canon _i ->
      let len, stack = Stack.pop_i32_to_int stack in
      (* TODO: check type of *default* *)
      let _default, stack = Stack.pop stack in
      let a = Array.init len (fun _i -> ()) in
      ok @@ Stack.push_array stack a
    | Array_new_canon_default _i ->
      let len, stack = Stack.pop_i32_to_int stack in
      let a = Array.init len (fun _i -> ()) in
      ok @@ Stack.push_array stack a
    | I31_new ->
      (* TODO *)
      ok stack

  let exec_expr env (e : Const.expr) : (Env.t' Value.t, string) Result.t =
    let* stack = list_fold_left (exec_instr env) Stack.empty e in
    match stack with
    | [] -> Error "type mismatch (const expr returning zero values)"
    | _ :: _ :: _ ->
      error_s "type mismatch (const expr returning more than one value)"
    | [ result ] -> Ok result
end

let load_from_module ls f (import : _ Types.imp) =
  match StringMap.find import.modul ls.by_name with
  | exception Not_found -> error_s "unknown module %s" import.modul
  | exports -> (
    match StringMap.find import.name (f exports) with
    | exception Not_found ->
      Log.debug "unknown import %s" import.name;
      if StringSet.mem import.name exports.defined_names then
        Error "incompatible import type (Link.load_from_module)"
      else Error "unknown import"
    | v -> Ok v )

let load_global (ls : state) (import : global_type Types.imp) :
  (global, string) Result.t =
  let* global = load_from_module ls (fun (e : exports) -> e.globals) import in
  let* () =
    match (fst import.desc, global.mut) with
    | Var, Const | Const, Var ->
      Error "incompatible import type (Link.load_global)"
    | Const, Const | Var, Var -> Ok ()
  in
  if snd import.desc <> global.typ then begin
    Error "incompatible import type (Link.load_global bis)"
  end
  else Ok global

let eval_global ls env
  (global : (Types.Simplified.global, global_type) Types.runtime) :
  (global, string) Result.t =
  match global with
  | Types.Local global ->
    let* value = Const_interp.exec_expr env global.init in
    let mut, typ = global.typ in
    let global : global = { value; label = global.id; mut; typ } in
    Ok global
  | Imported import -> load_global ls import

let eval_globals ls env globals : (Env.t, string) Result.t =
  Types.Named.fold
    (fun id global env ->
      let* env in
      let* global = eval_global ls env global in
      let env = Env.add_global id global env in
      Ok env )
    globals (Ok env)

(*
let eval_in_data (env : Env.t) (data : _ data') : (int, value) data' =
  let mode =
    match data.mode with
    | Data_passive -> Data_passive
    | Data_active (id, expr) ->
      let const = Const_interp.exec_expr env expr in
      Data_active (id, const)
  in
  { data with mode }
*)

let limit_is_included ~import ~imported =
  imported.min >= import.min
  &&
  match (imported.max, import.max) with
  | _, None -> true
  | None, Some _ -> false
  | Some i, Some j -> i <= j

let load_memory (ls : state) (import : limits Types.imp) :
  (Memory.t, string) Result.t =
  let* ({ Memory.limits = imported_limit; _ } as mem) =
    load_from_module ls (fun (e : exports) -> e.memories) import
  in
  if limit_is_included ~import:import.desc ~imported:imported_limit then Ok mem
  else
    error_s "incompatible import type for memory %s %s expected %a got %a"
      import.modul import.name Pp.limits import.desc Pp.limits imported_limit

let eval_memory ls (memory : (mem, limits) Types.runtime) :
  (Memory.t, string) Result.t =
  match memory with
  | Local (label, mem_type) -> ok @@ Memory.init ?label mem_type
  | Imported import -> load_memory ls import

let eval_memories ls env memories =
  Types.Named.fold
    (fun id mem env ->
      let* env in
      let* memory = eval_memory ls mem in
      let env = Env.add_memory id memory env in
      Ok env )
    memories (Ok env)

let table_types_are_compatible (import, (t1 : ref_type)) (imported, t2) =
  limit_is_included ~import ~imported && t1 = t2

let load_table (ls : state) (import : table_type Types.imp) :
  (table, string) Result.t =
  let typ : table_type = import.desc in
  let* t = load_from_module ls (fun (e : exports) -> e.tables) import in
  if table_types_are_compatible typ (t.limits, t.typ) then Ok t
  else
    error_s "incompatible import type for table %s %s expected %a got %a"
      import.modul import.name Pp.table_type typ Pp.table_type (t.limits, t.typ)

let eval_table ls (table : (_, table_type) Types.runtime) :
  (table, string) Result.t =
  match table with
  | Local (label, table_type) -> ok @@ Table.init ?label table_type
  | Imported import -> load_table ls import

let eval_tables ls env tables =
  Types.Named.fold
    (fun id table env ->
      let* env in
      let* table = eval_table ls table in
      let env = Env.add_table id table env in
      Ok env )
    tables (Ok env)

let func_types_are_compatible a b =
  (* TODO: copied from Simplify_bis.equal_func_types => should factorize *)
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

let load_func (ls : state) (import : func_type Types.imp) :
  (func, string) Result.t =
  let typ : func_type = import.desc in
  let* func = load_from_module ls (fun (e : exports) -> e.functions) import in
  let type' = Value.Func.typ func in
  if func_types_are_compatible typ type' then Ok func
  else Error "incompatible import type (Link.load_func)"

let eval_func ls (finished_env : Env.t') func : (func, string) Result.t =
  match func with
  | Types.Local func -> ok @@ Value.Func.wasm func finished_env
  | Imported import -> load_func ls import

let eval_functions ls (finished_env : Env.t') env functions =
  Types.Named.fold
    (fun id func env ->
      let* env in
      let* func = eval_func ls finished_env func in
      let env = Env.add_func id func env in
      Ok env )
    functions (Ok env)

let active_elem_expr ~offset ~length ~table ~elem =
  [ I32_const offset
  ; I32_const 0l
  ; I32_const length
  ; Table_init (table, elem)
  ; Elem_drop elem
  ]

let active_data_expr ~offset ~length ~mem ~data =
  if mem <> 0 then begin
    error_s "wrong memory id: %i@." mem
  end
  else
    Ok
      [ I32_const offset
      ; I32_const 0l
      ; I32_const length
      ; Memory_init data
      ; Data_drop data
      ]

let get_i32 = function Value.I32 i -> Ok i | _ -> Error "type mismatch"

let define_data env data =
  Types.Named.fold
    (fun id (data : Simplified.data) env_and_init ->
      let* env, init = env_and_init in
      let data' : Env.data = { value = data.init } in
      let env = Env.add_data id data' env in
      let* init =
        match data.mode with
        | Data_active (None, _) -> assert false
        | Data_active (Some mem, offset) ->
          let* offset = Const_interp.exec_expr env offset in
          let length = Int32.of_int @@ String.length data.init in
          let* offset = get_i32 offset in
          let* v = active_data_expr ~offset ~length ~mem ~data:id in
          ok @@ (v :: init)
        | Data_passive -> Ok init
      in
      Ok (env, init) )
    data
    (Ok (env, []))

let define_elem env elem =
  Types.Named.fold
    (fun id (elem : elem) env_and_inits ->
      let* env, inits = env_and_inits in
      let* init = list_map (Const_interp.exec_expr env) elem.init in
      let* init_as_ref =
        list_map
          (fun v ->
            match v with
            | Value.Ref v -> Ok v
            | _ -> Error "constant expression required" )
          init
      in
      let env =
        match elem.mode with
        | Elem_active _ | Elem_passive ->
          Env.add_elem id { value = Array.of_list init_as_ref } env
        | Elem_declarative ->
          (* Declarative element have no runtime value *)
          Env.add_elem id { value = [||] } env
      in
      let* inits =
        match elem.mode with
        | Elem_active (None, _) -> assert false
        | Elem_active (Some table, offset) ->
          let length = Int32.of_int @@ List.length init in
          let* offset = Const_interp.exec_expr env offset in
          let* offset = get_i32 offset in
          ok @@ (active_elem_expr ~offset ~length ~table ~elem:id :: inits)
        | Elem_passive | Elem_declarative -> Ok inits
      in
      Ok (env, inits) )
    elem
    (Ok (env, []))

let populate_exports env (exports : Types.Simplified.exports) :
  (exports, string) Result.t =
  let fill_exports get_env exports names =
    list_fold_left
      (fun (acc, names) (export : Types.Simplified.export) ->
        let* value = get_env env export.id in
        if StringSet.mem export.name names then Error "duplicate export name"
        else
          Ok
            ( StringMap.add export.name value acc
            , StringSet.add export.name names ) )
      (StringMap.empty, names) exports
  in
  let names = StringSet.empty in
  let* globals, names = fill_exports Env.get_global exports.global names in
  let* memories, names = fill_exports Env.get_memory exports.mem names in
  let* tables, names = fill_exports Env.get_table exports.table names in
  let* functions, names = fill_exports Env.get_func exports.func names in
  Ok { globals; memories; tables; functions; defined_names = names }

let modul (ls : state) ~name (modul : modul) =
  Log.debug "linking      ...@\n";
  let exception E of string in
  let raise_on_error = function Ok v -> v | Error msg -> raise (E msg) in
  let* env_and_init_active_data_and_elem =
    let rec env_and_init_active_data_and_elem =
      lazy
        (let env = Env.empty in
         let env =
           eval_functions ls finished_env env modul.func |> raise_on_error
         in
         let env = eval_globals ls env modul.global |> raise_on_error in
         let env = eval_memories ls env modul.mem |> raise_on_error in
         let env = eval_tables ls env modul.table |> raise_on_error in
         let env, init_active_data =
           define_data env modul.data |> raise_on_error
         in
         let env, init_active_elem =
           define_elem env modul.elem |> raise_on_error
         in
         (env, init_active_data, init_active_elem) )
    and finished_env =
      lazy
        (let env, _init_active_data, _init_active_elem =
           Lazy.force env_and_init_active_data_and_elem
         in
         env )
    in
    Ok env_and_init_active_data_and_elem
  in
  let* env, init_active_data, init_active_elem =
    try Ok (Lazy.force env_and_init_active_data_and_elem)
    with E msg -> Error msg
  in
  let* by_id_exports = populate_exports env modul.exports in
  let by_id =
    match modul.id with
    | None -> ls.by_id
    | Some id -> StringMap.add id by_id_exports ls.by_id
  in
  let by_name =
    match name with
    | None -> ls.by_name
    | Some name -> StringMap.add name by_id_exports ls.by_name
  in
  let start = Option.map (fun start_id -> [ Call start_id ]) modul.start in
  let start = Option.fold ~none:[] ~some:(fun s -> [ s ]) start in
  let to_run = (init_active_data @ init_active_elem) @ start in
  let module_to_run = { modul; env; to_run } in
  Ok (module_to_run, { by_id; by_name; last = Some by_id_exports })

let extern_module (ls : state) ~name (module_ : extern_module) =
  let functions =
    StringMap.map
      (fun f -> Value.Func.Extern f)
      (StringMap.of_seq (List.to_seq module_.functions))
  in
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
    ; defined_names
    }
  in
  { ls with by_name = StringMap.add name exports ls.by_name }

let register_module (ls : state) ~name ~(id : string option) :
  (state, string) Result.t =
  let* exports =
    match id with
    | Some id -> begin
      match StringMap.find_opt id ls.by_id with
      | None -> error_s "Unbound module id %s" id
      | Some e -> Ok e
    end
    | None -> (
      match ls.last with
      | Some e -> Ok e
      | None -> Error "No previous module to register" )
  in
  Ok { ls with by_name = StringMap.add name exports ls.by_name }
