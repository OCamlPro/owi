open Types

type module_ = Simplify.simplified_module

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let page_size = 65_536

module Memory = struct
  type t =
    { id : int
    ; label : string option
    ; mutable limits : mem_type
    ; mutable data : bytes
    }

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      !r

  let init ?label (typ : mem_type) : t =
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
    ; type_ : ref_type
    ; mutable data : 'env table
    }

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      !r

  let init ?label (typ : table_type) : 'env t =
    let limits, ref_type = typ in
    let null = Value.ref_null' ref_type in
    let table = Array.make limits.min null in
    { id = fresh (); label; limits; type_ = ref_type; data = table }

  let update table data = table.data <- data
end

module Global = struct
  type 'env t =
    { mutable value : 'env Value.t
    ; label : string option
    ; mut : Types.mut
    ; typ : Types.val_type
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

  let get_global (env : t) id : t' Global.t =
    match IMap.find_opt id env.globals with
    | None ->
      Log.debug "%a@." pp env;
      Log.err "unknown global"
    | Some v -> v

  let get_memory (env : t) id : Memory.t =
    match IMap.find_opt id env.memories with
    | None ->
      Log.debug "%a@." pp env;
      Log.err "unknown memory"
    | Some v -> v

  let get_table (env : t) id : t' Table.t =
    match IMap.find_opt id env.tables with
    | None ->
      Log.debug "%a@." pp env;
      Log.err "unknown table"
    | Some v -> v

  let get_func (env : t) id : t' Value.Func.t =
    match IMap.find_opt id env.functions with
    | None ->
      Log.debug "%a@." pp env;
      Log.err "unknown function %a" Format.pp_print_int id
    | Some v -> v

  let get_data (env : t) id : data =
    match IMap.find_opt id env.data with
    | None ->
      Log.debug "%a@." pp env;
      Log.err "unknown data"
    | Some v -> v

  let get_elem (env : t) id : t' elem =
    match IMap.find_opt id env.elem with
    | None ->
      Log.debug "%a@." pp env;
      Log.err "unknown elem"
    | Some v -> v
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
  { module_ : module_
  ; env : Env.t
  ; to_run : (int, func_type) expr' list
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
  open Types

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
    | I32_const n -> Stack.push_i32 stack n
    | I64_const n -> Stack.push_i64 stack n
    | F32_const f -> Stack.push_f32 stack f
    | F64_const f -> Stack.push_f64 stack f
    | I_binop (nn, op) -> exec_ibinop stack nn op
    | Ref_null t -> Stack.push stack (Value.ref_null t)
    | Ref_func f ->
      let value = Value.Ref (Funcref (Some (Env.get_func env f))) in
      Stack.push stack value
    | Global_get id -> Stack.push stack (Env.get_global env id).value

  let exec_expr env (e : Const.expr) : Env.t' Value.t =
    let stack = List.fold_left (exec_instr env) Stack.empty e in
    match stack with
    | [] -> Log.err "type mismatch (const expr returning zero values)"
    | _ :: _ :: _ ->
      Log.err "type mismatch (const expr returning more than one value %a)"
        Pp.Const.expr e
    | [ result ] -> result
end

let load_from_module ls f (import : _ Simplify.imp) =
  match StringMap.find import.module_ ls.by_name with
  | exception Not_found -> Log.err "unknown module %s" import.module_
  | exports -> (
    match StringMap.find import.name (f exports) with
    | exception Not_found ->
      Log.debug "unknown import %s" import.name;
      if StringSet.mem import.name exports.defined_names then
        Log.err "incompatible import type"
      else Log.err "unknown import"
    | v -> v )

let load_global (ls : state) (import : Types.global_type Simplify.imp) : global
    =
  let global = load_from_module ls (fun (e : exports) -> e.globals) import in
  ( match (fst import.desc, global.mut) with
  | Var, Const | Const, Var -> Log.err "incompatible import type"
  | Const, Const | Var, Var -> () );
  if snd import.desc <> global.typ then begin
    Log.err "incompatible import type"
  end;
  global

let eval_global ls env
    (global : (Const.expr global', Types.global_type) Simplify.runtime) : global
    =
  match global with
  | Simplify.Local global ->
    let value = Const_interp.exec_expr env global.init in
    let mut, typ = global.type_ in
    let global : global = { value; label = global.id; mut; typ } in
    global
  | Imported import -> load_global ls import

let eval_globals ls env globals : Env.t =
  Simplify.Named.fold
    (fun id global env ->
      let global = eval_global ls env global in
      let env = Env.add_global id global env in
      env )
    globals env

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

let load_memory (ls : state) (import : Types.limits Simplify.imp) : Memory.t =
  let ({ Memory.limits = imported_limit; _ } as mem) =
    load_from_module ls (fun (e : exports) -> e.memories) import
  in
  if limit_is_included ~import:import.desc ~imported:imported_limit then mem
  else
    Log.err "incompatible import type for memory %s %s expected %a got %a"
      import.module_ import.name Pp.Input.mem_type import.desc Pp.Input.mem_type
      imported_limit

let eval_memory ls (memory : (mem, Types.limits) Simplify.runtime) : Memory.t =
  match memory with
  | Local (label, mem_type) -> Memory.init ?label mem_type
  | Imported import -> load_memory ls import

let eval_memories ls env memories =
  Simplify.Named.fold
    (fun id mem env ->
      let memory = eval_memory ls mem in
      let env = Env.add_memory id memory env in
      env )
    memories env

let table_types_are_compatible (import, (t1 : ref_type)) (imported, t2) =
  limit_is_included ~import ~imported && t1 = t2

let load_table (ls : state) (import : Types.table_type Simplify.imp) : table =
  let type_ : table_type = import.desc in
  let t = load_from_module ls (fun (e : exports) -> e.tables) import in
  if table_types_are_compatible type_ (t.limits, t.type_) then t
  else
    Log.err "incompatible import type for table %s %s expected %a got %a"
      import.module_ import.name Pp.Input.table_type type_ Pp.Input.table_type
      (t.limits, t.type_)

let eval_table ls (table : (_, Types.table_type) Simplify.runtime) : table =
  match table with
  | Local (label, table_type) -> Table.init ?label table_type
  | Imported import -> load_table ls import

let eval_tables ls env tables =
  Simplify.Named.fold
    (fun id table env ->
      let table = eval_table ls table in
      let env = Env.add_table id table env in
      env )
    tables env

let func_types_are_compatible a b =
  (* TODO: copied from Simplify_bis.equal_func_types => should factorize *)
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

let load_func (ls : state) (import : func_type Simplify.imp) : func =
  let type_ : func_type = import.desc in
  let func = load_from_module ls (fun (e : exports) -> e.functions) import in
  let type' = Value.Func.typ func in
  if func_types_are_compatible type_ type' then func
  else Log.err "incompatible import type"

let eval_func ls (finished_env : Env.t') func : func =
  match func with
  | Simplify.Local func -> Value.Func.wasm func finished_env
  | Imported import -> load_func ls import

let eval_functions ls (finished_env : Env.t') env functions =
  Simplify.Named.fold
    (fun id func env ->
      let func = eval_func ls finished_env func in
      let env = Env.add_func id func env in
      env )
    functions env

let active_elem_expr ~offset ~length ~table ~elem =
  [ I32_const offset
  ; I32_const 0l
  ; I32_const length
  ; Table_init (table, elem)
  ; Elem_drop elem
  ]

let active_data_expr ~offset ~length ~mem ~data =
  if mem <> 0 then begin
    Log.err "wrong memory id: %i@." mem
  end;
  [ I32_const offset
  ; I32_const 0l
  ; I32_const length
  ; Memory_init data
  ; Data_drop data
  ]

let get_i32 = function Value.I32 i -> i | _ -> Log.err "type mismatch"

let define_data env data =
  Simplify.Named.fold
    (fun id data (env, init) ->
      let env = Env.add_data id { value = data.init } env in
      let init =
        match data.mode with
        | Data_active (mem, offset) ->
          let offset = Const_interp.exec_expr env offset in
          let length = Int32.of_int @@ String.length data.init in
          active_data_expr ~offset:(get_i32 offset) ~length ~mem ~data:id
          :: init
        | Data_passive -> init
      in
      (env, init) )
    data (env, [])

let define_elem env elem =
  Simplify.Named.fold
    (fun id (elem : _ elem') (env, inits) ->
      let init = List.map (Const_interp.exec_expr env) elem.init in
      let init_as_ref =
        List.map
          (fun v ->
            match v with
            | Value.Ref v -> v
            | _ -> Log.err "constant expression required" )
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
      let inits =
        match elem.mode with
        | Elem_active (table, offset) ->
          let length = Int32.of_int @@ List.length init in
          let offset = Const_interp.exec_expr env offset in
          active_elem_expr ~offset:(get_i32 offset) ~length ~table ~elem:id
          :: inits
        | Elem_passive | Elem_declarative -> inits
      in
      (env, inits) )
    elem (env, [])

let populate_exports env (exports : Simplify.exports) : exports =
  let fill_exports get_env exports names =
    List.fold_left
      (fun (acc, names) (export : Simplify.export) ->
        let value = get_env env export.id in
        if StringSet.mem export.name names then Log.err "duplicate export name";
        (StringMap.add export.name value acc, StringSet.add export.name names)
        )
      (StringMap.empty, names) exports
  in
  let names = StringSet.empty in
  let globals, names = fill_exports Env.get_global exports.global names in
  let memories, names = fill_exports Env.get_memory exports.mem names in
  let tables, names = fill_exports Env.get_table exports.table names in
  let functions, names = fill_exports Env.get_func exports.func names in
  { globals; memories; tables; functions; defined_names = names }

let module_ (module_ : module_) (ls : state) =
  Log.debug "linking      ...@\n";
  begin
    try
      let rec env_and_init_active_data_and_elem =
        lazy
          (let env = Env.empty in
           let env = eval_functions ls finished_env env module_.func in
           let env = eval_globals ls env module_.global in
           let env = eval_memories ls env module_.mem in
           let env = eval_tables ls env module_.table in
           let env, init_active_data = define_data env module_.data in
           let env, init_active_elem = define_elem env module_.elem in
           (env, init_active_data, init_active_elem) )
      and finished_env =
        lazy
          (let env, _init_active_data, _init_active_elem =
             Lazy.force env_and_init_active_data_and_elem
           in
           env )
      in
      let env, init_active_data, init_active_elem =
        Lazy.force env_and_init_active_data_and_elem
      in
      let by_id_exports = populate_exports env module_.exports in
      let by_id =
        (* TODO: this is not the actual module name *)
        match module_.id with
        | None -> ls.by_id
        | Some id -> StringMap.add id by_id_exports ls.by_id
      in
      let start = List.map (fun start_id -> Call start_id) module_.start in
      let to_run = init_active_data @ init_active_elem @ [ start ] in
      let module_to_run = { module_; env; to_run } in
      Ok
        ( module_to_run
        , { by_id; by_name = ls.by_name; last = Some by_id_exports } )
    with Failure msg -> Error msg
  end

let extern_module (name : string) (module_ : extern_module) (ls : state) : state
    =
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

let register_module (ls : state) ~name ~(id : string option) : state =
  let exports =
    match id with
    | Some id -> begin
      match StringMap.find_opt id ls.by_id with
      | None -> Log.err "Unbound module id %s" id
      | Some e -> e
    end
    | None -> (
      match ls.last with
      | Some e -> e
      | None -> Log.err "No previous module to register" )
  in
  { ls with by_name = StringMap.add name exports ls.by_name }
