open Types

type 'a runtime =
  | Local of 'a
  | Imported of int * int

type runtime_table = (ref_type * const option Array.t * int option) runtime

type runtime_global = (global_type * const) runtime

type runtime_memory = (bytes * int option) runtime

type runtime_func = func runtime

type module_ =
  { fields : module_field list
  ; seen_funcs : (string, int) Hashtbl.t
  ; datas : string array
  ; funcs : runtime_func array
  ; memories : runtime_memory array
  ; tables : runtime_table array
  ; types : func_type Array.t
  ; globals : runtime_global array
  ; elements : (ref_type * expr Array.t) Array.t
  ; exported_funcs : (string, int) Hashtbl.t
  ; exported_globals : (string, int) Hashtbl.t
  ; exported_memories : (string, int) Hashtbl.t
  ; exported_tables : (string, int) Hashtbl.t
  ; start : int option
  }

type action =
  | Invoke_indice of int * string * const list
  | Get of string option * string

type assert_ =
  | SAssert_return of action * result list
  | SAssert_trap of action * string
  | SAssert_trap_module of Types.module_ * string
  | SAssert_exhaustion of action * string
  | SAssert_malformed of Types.module_ * string
  | SAssert_malformed_quote of string list * string
  | SAssert_malformed_binary of string list * string
  | SAssert_invalid of Types.module_ * string
  | SAssert_invalid_quote of string list * string
  | SAssert_invalid_binary of string list * string
  | SAssert_unlinkable of Types.module_ * string

type cmd =
  | Module_indice of int
  | Assert of assert_
  | Register_indice of string * int
  | Action of action

type script = module_ Array.t * cmd list

let map_symb find_in_tbl = function
  | Raw i -> i
  | Symbolic id -> Uint32.of_int @@ find_in_tbl id

let map_symb_opt default find_in_tbl = function
  | None -> default
  | Some sym -> Uint32.to_int @@ map_symb find_in_tbl sym

let find_module name last seen =
  match name with
  | None -> begin
    match last with None -> failwith "no module defined" | Some i -> i
  end
  | Some mod_name -> begin
    match Hashtbl.find_opt seen mod_name with
    | None -> failwith @@ Format.sprintf "unknown module $%s" mod_name
    | Some i -> i
  end

let action last_module seen_modules = function
  | Invoke (mod_name, f, args) ->
    let i = find_module mod_name last_module seen_modules in
    Invoke_indice (i, f, args)
  | Get (id, n) ->
    (* TODO: simplify this *)
    Get (id, n)

let assert_ last_module seen_modules =
  let action = action last_module seen_modules in
  function
  | Assert_return (a, res) -> SAssert_return (action a, res)
  | Assert_trap (a, failure) -> SAssert_trap (action a, failure)
  | Assert_trap_module (module_, failure) ->
    SAssert_trap_module (module_, failure)
  | Assert_exhaustion (a, failure) -> SAssert_exhaustion (action a, failure)
  | Assert_malformed (module_, failure) -> SAssert_malformed (module_, failure)
  | Assert_malformed_quote (m, failure) -> SAssert_malformed_quote (m, failure)
  | Assert_malformed_binary (m, failure) -> SAssert_malformed_binary (m, failure)
  | Assert_invalid (module_, failure) -> SAssert_invalid (module_, failure)
  | Assert_invalid_quote (m, failure) -> SAssert_malformed_quote (m, failure)
  | Assert_invalid_binary (m, failure) -> SAssert_invalid_binary (m, failure)
  | Assert_unlinkable (m, s) -> SAssert_unlinkable (m, s)

type env =
  { curr_func : int
  ; curr_global : int
  ; curr_memory : int
  ; curr_table : int
  ; curr_type : int
  ; curr_element : int
  ; curr_data : int
  ; datas : string list
  ; funcs : runtime_func list
  ; globals : runtime_global list
  ; memories : runtime_memory list
  ; tables : runtime_table list
  ; types : func_type list
  ; start : int option
  }

let indice_to_int = function
  | Raw i -> Uint32.to_int i
  | Symbolic i -> failwith @@ Format.sprintf "indice_to_int: %s" i

let find_id tbl x = function
  | Raw i -> Uint32.to_int i
  | Symbolic i -> (
    match Hashtbl.find_opt tbl i with
    | None -> failwith @@ Format.asprintf "unbound %s id %a" x Pp.id i
    | Some i -> i )

let find_ind tbl x ind =
  match Hashtbl.find_opt tbl ind with
  | None -> failwith @@ Format.asprintf "unbound %s indice %s" x ind
  | Some i -> i

let rec get_table (modules : module_ array) tables i =
  match tables.(i) with
  | Local (rt, tbl, max) ->
    (rt, tbl, max, fun tbl -> tables.(i) <- Local (rt, tbl, max))
  | Imported (m, i) -> get_table modules modules.(m).tables i

let rec get_global (modules : module_ array) globals i =
  match globals.(i) with
  | Local (gt, g) -> (gt, g, fun g -> globals.(i) <- Local (gt, g))
  | Imported (m, i) -> get_global modules modules.(m).globals i

let rec get_memory (modules : module_ array) memories i =
  match memories.(i) with
  | Local (m, max) -> (m, max, fun m max -> memories.(i) <- Local (m, max))
  | Imported (m, i) -> get_memory modules modules.(m).memories i

let rec get_func (modules : module_ array) funcs i =
  match funcs.(i) with
  (* TODO: do we need set somewhere ? *)
  | Local f -> (f, fun f -> funcs.(i) <- Local f)
  | Imported (m, i) -> get_func modules modules.(m).funcs i

let mk_module registered_modules modules m =
  let modules = Array.of_list @@ List.rev modules in
  let exported_funcs = Hashtbl.create 512 in
  let exported_globals = Hashtbl.create 512 in
  let exported_memories = Hashtbl.create 512 in
  let exported_tables = Hashtbl.create 512 in

  let seen_types = Hashtbl.create 512 in
  let find_type = find_id seen_types "type" in

  let seen_funcs = Hashtbl.create 512 in
  let find_func = find_ind seen_funcs "func" in

  let seen_memories = Hashtbl.create 512 in
  let find_memory = find_ind seen_memories "memory" in

  let seen_globals = Hashtbl.create 512 in
  let find_global = find_ind seen_globals "global" in

  let seen_tables = Hashtbl.create 512 in
  let find_table = find_ind seen_tables "table" in

  let seen_elements = Hashtbl.create 512 in
  let find_element = find_ind seen_elements "element" in

  let seen_datas = Hashtbl.create 512 in
  let find_data = find_ind seen_datas "data" in

  let find_module = find_ind registered_modules "module" in

  let const_expr find_global globals = function
    | [ I32_const n ] -> Const_I32 n
    | [ I64_const n ] -> Const_I64 n
    | [ F32_const f ] -> Const_F32 f
    | [ F64_const f ] -> Const_F64 f
    | [ Ref_null rt ] -> Const_null rt
    | [ Global_get i ] ->
      let _gt, e, _set =
        get_global modules globals (Uint32.to_int @@ find_global i)
      in
      e
    | [ Ref_func ind ] -> Const_host (Uint32.to_int @@ map_symb find_func ind)
    | e -> failwith @@ Format.asprintf "TODO global expression: `%a`" Pp.expr e
  in

  let env =
    List.fold_left
      (fun env -> function
        | MStart indice ->
          begin
            match env.start with
            | None -> ()
            | Some _id -> failwith "multiple start functions are not allowed"
          end;
          let indice = Uint32.to_int @@ map_symb find_func indice in
          { env with start = Some indice }
        | MFunc f ->
          let curr_func = env.curr_func + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_funcs id curr_func) f.id;
          let funcs = Local f :: env.funcs in
          { env with curr_func; funcs }
        | MGlobal g ->
          let curr_global = env.curr_global + 1 in
          Option.iter (fun id -> Hashtbl.add seen_globals id curr_global) g.id;
          let init =
            const_expr (map_symb find_global)
              (Array.of_list @@ List.rev env.globals)
              g.init
          in
          let globals = Local (g.type_, init) :: env.globals in
          { env with curr_global; globals }
        | MExport { name; desc } ->
          begin
            match desc with
            | Export_func indice ->
              let i = map_symb_opt env.curr_func find_func indice in
              Hashtbl.replace exported_funcs name i
            | Export_table indice ->
              let i = map_symb_opt env.curr_table find_table indice in
              Hashtbl.replace exported_tables name i
            | Export_global indice ->
              let i = map_symb_opt env.curr_global find_global indice in
              Hashtbl.replace exported_globals name i
            | Export_mem indice ->
              let i = map_symb_opt env.curr_memory find_memory indice in
              Hashtbl.replace exported_memories name i
          end;
          env
        | MImport { desc = Import_func (id, _t); module_; name } ->
          let curr_func = env.curr_func + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_funcs id curr_func) id;
          let module_indice = find_module module_ in
          let func_indice =
            match
              Hashtbl.find_opt modules.(module_indice).exported_funcs name
            with
            | None -> failwith @@ Format.sprintf "Unbound imported func %s" name
            | Some ind -> ind
          in
          let funcs = Imported (module_indice, func_indice) :: env.funcs in
          { env with curr_func; funcs }
        | MImport { desc = Import_mem (id, _t); module_; name } ->
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          let module_indice = find_module module_ in
          let mem_indice =
            match
              Hashtbl.find_opt modules.(module_indice).exported_memories name
            with
            | None -> failwith "Unbound imported memory"
            | Some ind -> ind
          in
          let memories = Imported (module_indice, mem_indice) :: env.memories in
          { env with curr_memory; memories }
        | MImport { desc = Import_table (id, _t); module_; name } ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_tables id curr_table) id;
          (* TODO: fix this *)
          if module_ = "spectest" && name = "table" then () else ();
          let module_indice = find_module module_ in
          let table_indice =
            match
              Hashtbl.find_opt modules.(module_indice).exported_tables name
            with
            | None -> failwith "Unbound imported table"
            | Some ind -> ind
          in
          let tables = Imported (module_indice, table_indice) :: env.tables in
          { env with curr_table; tables }
        | MImport { desc = Import_global (id, _t); module_; name } ->
          let curr_global = env.curr_global + 1 in
          Option.iter (fun id -> Hashtbl.add seen_globals id curr_global) id;
          let module_indice = find_module module_ in
          let global_indice =
            match
              Hashtbl.find_opt modules.(module_indice).exported_globals name
            with
            | None ->
              failwith @@ Format.sprintf "Unbound imported global %s" name
            | Some ind -> ind
          in
          let globals =
            Imported (module_indice, global_indice) :: env.globals
          in
          { env with curr_global; globals }
        | MMem (id, { min; max }) ->
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          let max = Option.map Int32.to_int max in
          let new_bytes =
            Bytes.make (Int32.to_int min * page_size) (Char.chr 0)
          in
          let memories = Local (new_bytes, max) :: env.memories in
          { env with curr_memory; memories }
        | MTable (id, ({ min; max }, rt)) ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.add seen_tables id curr_table) id;
          let a = Array.make (Int32.to_int min) None in
          let tbl = Local (rt, a, Option.map Int32.to_int max) in
          let tables = tbl :: env.tables in
          { env with curr_table; tables }
        | MType (id, t) ->
          let curr_type = env.curr_type + 1 in
          let types = t :: env.types in
          Option.iter (fun id -> Hashtbl.add seen_types id curr_type) id;
          { env with curr_type; types }
        | MElem e ->
          let curr_element = env.curr_element + 1 in
          Option.iter (fun id -> Hashtbl.add seen_elements id curr_element) e.id;
          { env with curr_element }
        | MData data ->
          let curr_data = env.curr_data + 1 in
          Option.iter (fun id -> Hashtbl.add seen_datas id curr_data) data.id;
          let data =
            match data.mode with
            | Data_passive -> data.init
            | Data_active (indice, expr) -> (
              let indice = map_symb_opt env.curr_memory find_memory indice in
              let globals = List.rev env.globals in
              let globals = Array.of_list globals in
              match const_expr (map_symb find_global) globals expr with
              | Const_I32 offset ->
                let mem_bytes, _max, _set =
                  get_memory modules
                    (Array.of_list @@ List.rev env.memories)
                    indice
                in
                let len = String.length data.init in
                Bytes.blit_string data.init 0 mem_bytes (Int32.to_int offset)
                  len;
                ""
              | c ->
                failwith @@ Format.asprintf "TODO data_offset `%a`" Pp.const c )
          in
          { env with datas = data :: env.datas; curr_data } )
      { curr_func = -1
      ; curr_global = -1
      ; curr_memory = -1
      ; curr_table = -1
      ; curr_type = -1
      ; curr_element = -1
      ; curr_data = -1
      ; datas = []
      ; funcs = []
      ; globals = []
      ; memories = []
      ; tables = []
      ; types = []
      ; start = None
      }
      m.Types.fields
  in

  let start = env.start in
  let datas = List.rev env.datas |> Array.of_list in
  let globals = List.rev env.globals |> Array.of_list in
  let tables = List.rev env.tables |> Array.of_list in
  let types = List.rev env.types |> Array.of_list in
  let memories = List.rev env.memories |> Array.of_list in
  let funcs = List.rev env.funcs |> Array.of_list in

  let _curr_table, fields, segments =
    List.fold_left
      (fun (curr_table, fields, segments) -> function
        | MTable _ as f -> (curr_table + 1, f :: fields, segments)
        | MElem e as f -> (
          match e.mode with
          | Elem_passive ->
            let segment =
              List.map
                (List.map (function
                  | Ref_func i -> Ref_func (Raw (map_symb find_func i))
                  | Ref_null rt -> Ref_null rt
                  | i ->
                    failwith @@ Format.asprintf "TODO expr: `%a`" Pp.instr i )
                  )
                e.init
            in
            (curr_table, f :: fields, (e.type_, segment) :: segments)
          | Elem_active (ti, offset) ->
            let ti = Option.map (map_symb find_table) ti in
            let ti = Option.map (fun i -> Raw i) ti in
            let f =
              List.map (function
                | Ref_func i -> Ref_func (Raw (map_symb find_func i))
                | Ref_null rt -> Ref_null rt
                | I32_const n -> I32_const n
                | Global_get i -> Global_get (Raw (map_symb find_global i))
                | i -> failwith @@ Format.asprintf "TODO expr: `%a`" Pp.instr i )
            in
            let init = List.map f e.init in
            let offset = f offset in
            ( curr_table
            , MElem { e with mode = Elem_active (ti, offset); init } :: fields
            , (e.type_, []) :: segments )
          | Elem_declarative ->
            (curr_table, f :: fields, (e.type_, []) :: segments) )
        | f -> (curr_table, f :: fields, segments) )
      (-1, [], []) m.Types.fields
  in
  let fields = List.rev fields in

  let segments = List.rev segments |> Array.of_list in
  let segments = Array.map (fun (rt, l) -> (rt, Array.of_list l)) segments in

  let funcs =
    Array.map
      (function
        | Local f ->
          let local_tbl = Hashtbl.create 512 in
          let find_local id =
            match Hashtbl.find_opt local_tbl id with
            | None -> failwith @@ Format.sprintf "unbound local %s" id
            | Some i -> i
          in
          let type_f =
            match f.type_f with
            | Bt_ind ind -> types.(find_type ind)
            | Bt_raw t -> t
          in
          (* adding params and locals to the locals table *)
          let locals = fst type_f @ f.locals in
          List.iteri
            (fun i (id, _t) ->
              Option.iter (fun id -> Hashtbl.add local_tbl id i) id )
            locals;

          (* block_ids handling *)
          let find_block_id id l =
            let pos = ref (-1) in
            begin
              try
                List.iteri
                  (fun i n ->
                    if n = Some id then begin
                      pos := i;
                      raise Exit
                    end )
                  l
              with Exit -> ()
            end;

            if !pos = -1 then failwith @@ Format.sprintf "unbound label %s" id
            else Uint32.of_int !pos
          in

          let bt_to_raw =
            Option.map (function
              | Bt_ind ind -> Bt_raw types.(find_type ind)
              | t -> t )
          in

          (* handling an expression *)
          let rec body block_ids = function
            | Br_table (ids, id) ->
              let f = function
                | Symbolic id -> Raw (find_block_id id block_ids)
                | Raw id -> Raw id
              in
              Br_table (Array.map f ids, f id)
            | Br_if (Symbolic id) -> Br_if (Raw (find_block_id id block_ids))
            | Br (Symbolic id) -> Br (Raw (find_block_id id block_ids))
            | Call id -> Call (Raw (map_symb find_func id))
            | Local_set id -> Local_set (Raw (map_symb find_local id))
            | Local_get id -> Local_get (Raw (map_symb find_local id))
            | Local_tee id -> Local_tee (Raw (map_symb find_local id))
            | If_else (id, bt, e1, e2) ->
              let bt = bt_to_raw bt in
              let block_ids = id :: block_ids in
              If_else (id, bt, expr e1 block_ids, expr e2 block_ids)
            | Loop (id, bt, e) ->
              let bt = bt_to_raw bt in
              Loop (id, bt, expr e (id :: block_ids))
            | Block (id, bt, e) ->
              let bt = bt_to_raw bt in
              Block (id, bt, expr e (id :: block_ids))
            | Call_indirect (tbl_i, bt) ->
              let bt = Option.get @@ bt_to_raw (Some bt) in
              Call_indirect (Raw (map_symb find_table tbl_i), bt)
            | Global_set id -> Global_set (Raw (map_symb find_global id))
            | Global_get id -> Global_get (Raw (map_symb find_global id))
            | Ref_func id -> Ref_func (Raw (map_symb find_func id))
            | Table_size id -> Table_size (Raw (map_symb find_table id))
            | Table_get id -> Table_get (Raw (map_symb find_table id))
            | Table_set id -> Table_set (Raw (map_symb find_table id))
            | Table_grow id -> Table_grow (Raw (map_symb find_table id))
            | Table_init (i, i') ->
              Table_init
                (Raw (map_symb find_table i), Raw (map_symb find_element i'))
            | Table_fill id -> Table_fill (Raw (map_symb find_table id))
            | Memory_init id -> Memory_init (Raw (map_symb find_data id))
            | Data_drop id -> Data_drop (Raw (map_symb find_data id))
            | i -> i
          and expr e block_ids = List.map (body block_ids) e in
          let body = expr f.body [] in
          Local { f with body; type_f = Bt_raw type_f }
        | Imported _ as f -> f )
      funcs
  in

  { fields
  ; funcs
  ; seen_funcs = Hashtbl.create 512
  ; memories
  ; tables
  ; types
  ; globals
  ; elements = segments
  ; start
  ; datas
  ; exported_funcs
  ; exported_globals
  ; exported_memories
  ; exported_tables
  }

let script script =
  let script =
    Module
      { id = Some "spectest"
      ; fields =
          [ MMem (Some "memory", { min = 1l; max = None })
          ; MFunc
              { type_f = Bt_raw ([ (None, Num_type I32) ], [])
              ; locals = []
              ; body = []
              ; id = Some "print"
              }
          ; MFunc
              { type_f = Bt_raw ([ (None, Num_type I32) ], [])
              ; locals = []
              ; body = []
              ; id = Some "print_i32"
              }
          ; MFunc
              { type_f = Bt_raw ([ (None, Num_type I64) ], [])
              ; locals = []
              ; body = []
              ; id = Some "print_i64"
              }
          ; MFunc
              { type_f = Bt_raw ([ (None, Num_type F32) ], [])
              ; locals = []
              ; body = []
              ; id = Some "print_f32"
              }
          ; MFunc
              { type_f = Bt_raw ([ (None, Num_type F64) ], [])
              ; locals = []
              ; body = []
              ; id = Some "print_f64"
              }
          ; MFunc
              { type_f =
                  Bt_raw ([ (None, Num_type I32); (None, Num_type F32) ], [])
              ; locals = []
              ; body = []
              ; id = Some "print_i32_f32"
              }
          ; MFunc
              { type_f =
                  Bt_raw ([ (None, Num_type F64); (None, Num_type F64) ], [])
              ; locals = []
              ; body = []
              ; id = Some "print_f64_f64"
              }
          ; MTable (Some "table", ({ min = 10l; max = Some 20l }, Func_ref))
          ; MGlobal
              { type_ = (Var, Num_type I32)
              ; init = [ I32_const 666l ]
              ; id = Some "global_i32"
              }
          ; MGlobal
              { type_ = (Var, Num_type I64)
              ; init = [ I64_const 666L ]
              ; id = Some "global_i64"
              }
          ; MGlobal
              { type_ = (Var, Num_type F32)
              ; init = [ F32_const Float32.zero ]
              ; id = Some "global_f32"
              }
          ; MGlobal
              { type_ = (Var, Num_type F64)
              ; init = [ F64_const Float64.zero ]
              ; id = Some "global_f64"
              }
          ; MExport
              { name = "memory"; desc = Export_mem (Some (Symbolic "memory")) }
          ; MExport
              { name = "table"; desc = Export_table (Some (Symbolic "table")) }
          ; MExport
              { name = "print"; desc = Export_func (Some (Symbolic "print")) }
          ; MExport
              { name = "print_i32"
              ; desc = Export_func (Some (Symbolic "print_i32"))
              }
          ; MExport
              { name = "print_f32"
              ; desc = Export_func (Some (Symbolic "print_f32"))
              }
          ; MExport
              { name = "print_i64"
              ; desc = Export_func (Some (Symbolic "print_i64"))
              }
          ; MExport
              { name = "print_f64"
              ; desc = Export_func (Some (Symbolic "print_f64"))
              }
          ; MExport
              { name = "print_i32_f32"
              ; desc = Export_func (Some (Symbolic "print_i32_f32"))
              }
          ; MExport
              { name = "print_f64_f64"
              ; desc = Export_func (Some (Symbolic "print_f64_f64"))
              }
          ; MExport
              { name = "global_i32"
              ; desc = Export_global (Some (Symbolic "global_i32"))
              }
          ; MExport
              { name = "global_i64"
              ; desc = Export_global (Some (Symbolic "global_i64"))
              }
          ; MExport
              { name = "global_f32"
              ; desc = Export_global (Some (Symbolic "global_f32"))
              }
          ; MExport
              { name = "global_f64"
              ; desc = Export_global (Some (Symbolic "global_f64"))
              }
          ]
      }
    :: Register ("spectest", Some "spectest")
    :: script
  in

  let _curr_module, modules, script =
    let seen_modules = Hashtbl.create 512 in
    let registered_modules = Hashtbl.create 512 in
    List.fold_left
      (fun (curr_module, modules, script) -> function
        | Module m ->
          let curr_module = curr_module + 1 in
          Option.iter
            (fun id -> Hashtbl.replace seen_modules id curr_module)
            m.id;
          let cmd = Module_indice curr_module in
          let modules = mk_module registered_modules modules m :: modules in
          (curr_module, modules, cmd :: script)
        | Assert a ->
          let cmd = Assert (assert_ (Some curr_module) seen_modules a) in
          (curr_module, modules, cmd :: script)
        | Register (name, mod_name) ->
          let indice = find_module mod_name (Some curr_module) seen_modules in
          Hashtbl.replace registered_modules name indice;
          let cmd = Register_indice (name, indice) in
          (curr_module, modules, cmd :: script)
        | Action a ->
          let cmd = Action (action (Some curr_module) seen_modules a) in
          (curr_module, modules, cmd :: script) )
      (-1, [], []) script
  in

  let script = List.rev script in
  let modules = List.rev modules |> Array.of_list in

  (script, modules)
