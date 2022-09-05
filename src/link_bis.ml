open Types
module S = Simplify_bis

type module_ = S.result

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

module Memory = struct
  type mem_id = Mid of int [@@unboxed]

  type mem = bytes

  type t =
    | Memory of
        { id : mem_id
        ; label : string option
        ; limits : mem_type (* TODO: min is useless: part of data, remove *)
        ; mutable data : mem
        }

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      Mid !r

  let init ?label (typ : mem_type) : t =
    let data = Bytes.make (Types.page_size * typ.min) '\x00' in
    Memory { id = fresh (); label; limits = typ; data }

  let update_memory (Memory mem) data = mem.data <- data
end

type memory = Memory.t

module Table = struct
  type table_id = Tid of int

  (* TODO: Value.ref_value array, gadt to constraint to the right ref_type ? *)
  type 'env table = 'env Value.ref_value array

  type 'env t =
    | Table of
        { id : table_id
        ; label : string option
        ; limits : limits
        ; type_ : ref_type
        ; mutable data : 'env table
        }

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      Tid !r

  let init ?label (typ : table_type) : 'env t =
    let limits, ref_type = typ in
    let null = Value.ref_null' ref_type in
    let table = Array.make limits.min null in
    Table { id = fresh (); label; limits; type_ = ref_type; data = table }

  let update (Table table) data = table.data <- data

  let pp_id fmt (Tid i) = Format.fprintf fmt "%i" i

  let pp fmt (Table t) =
    Format.fprintf fmt "Table{%a %a %i}" pp_id t.id Pp.pp_id t.label
      (Array.length t.data)
end

module Global = struct
  type 'env t =
    { mutable value : 'env Value.t
    ; label : string option
    ; mut : Types.mut
    ; typ : Types.val_type
    }
end

module Index = struct
  type t = S.index

  let pp fmt (I id) = Format.fprintf fmt "%i" id

  let compare = compare
end

(* TODO efficient imap for contiguous index (array) *)
module IMap = Map.Make (Index)

module Env = struct
  type data = { mutable value : string }

  let drop_data data = data.value <- ""

  type 'env elem = { mutable value : 'env Value.ref_value array }

  let drop_elem elem = elem.value <- [||]

  type t =
    { globals : t' Global.t IMap.t
    ; memories : memory IMap.t
    ; tables : t' Table.t IMap.t
    ; functions : t' Value.func IMap.t
    ; data : data IMap.t
    ; elem : t' elem IMap.t
    }

  and t' = t lazy_t

  let pp fmt t =
    let elt fmt (id, (global : 'a Global.t)) =
      Format.fprintf fmt "%a -> %a" Index.pp id Value.pp global.value
    in
    Format.fprintf fmt "@[<hov 2>{@ %a@ }@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         elt )
      (IMap.bindings t.globals)

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
      Debug.debugerr "%a@." pp env;
      failwith "unknown global"
    | Some v -> v

  let get_memory (env : t) id : Memory.t =
    match IMap.find_opt id env.memories with
    | None ->
      Debug.debugerr "%a@." pp env;
      failwith "unknown memory"
    | Some v -> v

  let get_table (env : t) id : t' Table.t =
    match IMap.find_opt id env.tables with
    | None ->
      Debug.debugerr "%a@." pp env;
      failwith "unknown table"
    | Some v -> v

  let get_func (env : t) id : t' Value.func =
    match IMap.find_opt id env.functions with
    | None ->
      Debug.debugerr "%a@." pp env;
      failwith (Format.asprintf "unknown function %a" Index.pp id)
    | Some v -> v

  let get_data (env : t) id : data =
    match IMap.find_opt id env.data with
    | None ->
      Debug.debugerr "%a@." pp env;
      failwith "unknown data"
    | Some v -> v

  let get_elem (env : t) id : t' elem =
    match IMap.find_opt id env.elem with
    | None ->
      Debug.debugerr "%a@." pp env;
      failwith "unknown elem"
    | Some v -> v
end

type global = Env.t' Global.t

type table = Env.t' Table.t

type func = Env.t' Value.func

type value = Env.t' Value.t

type exports =
  { globals : global StringMap.t
  ; memories : memory StringMap.t
  ; tables : table StringMap.t
  ; functions : func StringMap.t
  }

type module_to_run =
  { module_ : module_
  ; env : Env.t
  ; to_run : (S.index, func_type) expr' list
  }

type link_state =
  { by_name : exports StringMap.t
  ; by_id : exports StringMap.t
  ; last : exports option
  }

type extern_module = { functions : (string * Value.extern_func) list }

let empty_state =
  { by_name = StringMap.empty; by_id = StringMap.empty; last = None }

module Const_interp = struct
  open Types

  type env = Env.t

  module Stack = Stack_bis

  let exec_ibinop stack nn (op : Const.ibinop) =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      Stack.push_i32 stack
        (let open Int32 in
        match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2)
    | S64 ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      Stack.push_i64 stack
        (let open Int64 in
        match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2)

  let exec_instr (env : env) (stack : Env.t' Stack_bis.t) (instr : Const.instr)
      =
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
    | [] -> failwith "const expr returning zero values"
    | _ :: _ :: _ ->
      failwith
        (Format.asprintf "const expr returning more than one value %a"
           Pp.Const.expr e )
    | [ result ] -> result
end

let load_from_module ls f (import : _ S.imp) =
  match StringMap.find import.module_ ls.by_name with
  | exception Not_found -> failwith ("unknown module " ^ import.module_)
  | exports -> (
    match StringMap.find import.name (f exports) with
    | exception Not_found -> failwith ("unknown name " ^ import.name)
    | v -> v )

let load_global (ls : link_state) (import : S.global_import S.imp) : global =
  match import.desc with
  | Var, _ -> failwith "constant expression required"
  | Const, _ -> load_from_module ls (fun (e : exports) -> e.globals) import

let eval_global ls env
    (global : ((S.index, Const.expr) global', S.global_import) S.runtime) :
    global =
  match global with
  | S.Local global ->
    let value = Const_interp.exec_expr env global.init in
    let mut, typ = global.type_ in
    let global : global = { value; label = global.id; mut; typ } in
    global
  | S.Imported import -> load_global ls import

let eval_globals ls globals : Env.t =
  S.Fields.fold
    (fun id global env ->
      let global = eval_global ls env global in
      let env = Env.add_global id global env in
      env )
    globals Env.empty

let eval_in_data (env : Env.t) (data : _ data') : (S.index, value) data' =
  let mode =
    match data.mode with
    | Data_passive -> Data_passive
    | Data_active (id, expr) ->
      let const = Const_interp.exec_expr env expr in
      Data_active (id, const)
  in
  { data with mode }

let limit_is_included l ~into =
  into.min <= l.min
  &&
  match into.max with
  | None -> true
  | Some into_max -> (
    match l.max with None -> false | Some l_max -> into_max >= l_max )

let load_memory (ls : link_state) (import : S.mem_import S.imp) : memory =
  let limits : mem_type = import.desc in
  let (Memory { limits = limits'; _ } as mem) =
    load_from_module ls (fun (e : exports) -> e.memories) import
  in
  if limit_is_included limits' ~into:limits then mem
  else failwith "incompatible memory limits"

let eval_memory ls (memory : (mem, S.mem_import) S.runtime) : Memory.t =
  match memory with
  | Local (label, mem_type) -> Memory.init ?label mem_type
  | Imported import -> load_memory ls import

let eval_memories ls env memories =
  S.Fields.fold
    (fun id mem env ->
      let memory = eval_memory ls mem in
      let env = Env.add_memory id memory env in
      env )
    memories env

let table_types_are_compatible (l1, (t1 : ref_type)) (l2, t2) =
  limit_is_included l2 ~into:l1 && t1 = t2

let load_table (ls : link_state) (import : S.table_import S.imp) : table =
  let type_ : table_type = import.desc in
  let (Table t as table) =
    load_from_module ls (fun (e : exports) -> e.tables) import
  in
  if table_types_are_compatible type_ (t.limits, t.type_) then table
  else failwith "incompatible table import"

let eval_table ls (table : (_, S.table_import) S.runtime) : table =
  match table with
  | Local (label, table_type) -> Table.init ?label table_type
  | Imported import -> load_table ls import

let eval_tables ls env tables =
  S.Fields.fold
    (fun id table env ->
      let table = eval_table ls table in
      let env = Env.add_table id table env in
      env )
    tables env

let func_types_are_compatible _t1 _t2 =
  (* TODO *)
  true

let load_func (ls : link_state) (import : func_type S.imp) : func =
  let type_ : func_type = import.desc in
  let func = load_from_module ls (fun (e : exports) -> e.functions) import in
  let type' = Value.Func.type_ func in
  if func_types_are_compatible type_ type' then func
  else failwith "incompatible function import "

let eval_func ls (finished_env : Env.t') (func : (S.func, func_type) S.runtime)
    : func =
  match func with
  | Local func -> Value.Func.wasm func finished_env
  | Imported import -> load_func ls import

let eval_functions ls (finished_env : Env.t') env functions =
  S.Fields.fold
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
  let (I mem_id) = mem in
  if mem_id <> 0 then begin
    failwith (Printf.sprintf "wrong memory id: %i@." mem_id)
  end;
  [ I32_const offset
  ; I32_const 0l
  ; I32_const length
  ; Memory_init data
  ; Data_drop data
  ]

let get_i32 = function Value.I32 i -> i | _ -> failwith "Not an i32 const"

let define_data env data =
  S.Fields.fold
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
  S.Fields.fold
    (fun id (elem : _ elem') (env, inits) ->
      let init = List.map (Const_interp.exec_expr env) elem.init in
      let init_as_ref =
        List.map
          (fun v ->
            match v with
            | Value.Ref v -> v
            | _ -> failwith "constant expression required" )
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

let populate_exports env (exports : S.index S.exports) : exports =
  let fill_exports get_env exports names =
    List.fold_left
      (fun (acc, names) (export : _ S.export) ->
        let value = get_env env export.id in
        if StringSet.mem export.name names then failwith "duplicate export name";
        (StringMap.add export.name value acc, StringSet.add export.name names)
        )
      (StringMap.empty, names) exports
  in
  let names = StringSet.empty in
  let globals, names = fill_exports Env.get_global exports.global names in
  let memories, names = fill_exports Env.get_memory exports.mem names in
  let tables, names = fill_exports Env.get_table exports.table names in
  let functions, _names = fill_exports Env.get_func exports.func names in
  { globals; memories; tables; functions }

let link_module (module_ : module_) (ls : link_state) :
    module_to_run * link_state =
  let rec env_and_init_active_data_and_elem =
    lazy
      ( Debug.debug Format.err_formatter "LINK %a@\n" Pp.pp_id module_.id;
        let env = eval_globals ls module_.global in
        let env = eval_memories ls env module_.mem in
        let env = eval_tables ls env module_.table in
        let env = eval_functions ls finished_env env module_.func in
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
  Debug.debug Format.err_formatter "EVAL %a@\n" Env.pp env;
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
  (module_to_run, { by_id; by_name = ls.by_name; last = Some by_id_exports })

let link_extern_module (name : string) (module_ : extern_module)
    (ls : link_state) : link_state =
  Debug.debug Format.err_formatter "LINK EXTERN %s@\n" name;
  let functions =
    StringMap.map Value.Func.extern
      (StringMap.of_seq (List.to_seq module_.functions))
  in
  let exports =
    { functions
    ; globals = StringMap.empty
    ; memories = StringMap.empty
    ; tables = StringMap.empty
    }
  in
  { ls with by_name = StringMap.add name exports ls.by_name }

let register_module (ls : link_state) ~name ~(id : string option) : link_state =
  let exports =
    match id with
    | Some id -> begin
      match StringMap.find_opt id ls.by_id with
      | None -> failwith (Printf.sprintf "Unbound module id %s" id)
      | Some e -> e
    end
    | None -> (
      match ls.last with
      | Some e -> e
      | None -> failwith "No previous module to register" )
  in
  { ls with by_name = StringMap.add name exports ls.by_name }
