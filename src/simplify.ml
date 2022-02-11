open Types

type 'a runtime =
  | Local of 'a
  | Imported of int * indice

type runtime_table =
  (ref_type * (int * const) option array * int option) runtime

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
  ; globals : runtime_global array
  ; globals_tmp : (global_type * expr) runtime array
  ; types : func_type array
  ; elements : (ref_type * const array) array
  ; exported_funcs : (string, int) Hashtbl.t
  ; exported_globals : (string, int) Hashtbl.t
  ; exported_memories : (string, int) Hashtbl.t
  ; exported_tables : (string, int) Hashtbl.t
  ; start : int option
  ; should_trap : string option
  }

type action =
  | Invoke_indice of int * string * const list
  | Get_indice of int * string

type assert_ =
  | SAssert_return of action * result list
  | SAssert_trap of action * string
  | SAssert_exhaustion of action * string
  | SAssert_malformed of Types.module_ * string
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

let map_symb find_in_tbl = function Raw i -> i | Symbolic id -> find_in_tbl id

let map_symb_raw find_in_tbl sym = Raw (map_symb find_in_tbl sym)

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
  | Get (mod_name, n) ->
    let i = find_module mod_name last_module seen_modules in
    Get_indice (i, n)

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
  ; globals_tmp : (global_type * expr) runtime list
  ; memories : runtime_memory list
  ; tables : runtime_table list
  ; types : func_type list
  ; start : int option
  }

let find_id tbl x = function
  | Raw i -> i
  | Symbolic i -> (
    match Hashtbl.find_opt tbl i with
    | None -> failwith @@ Format.asprintf "unbound %s id %a" x Pp.id i
    | Some i -> i )

let find_ind tbl x ind =
  match Hashtbl.find_opt tbl ind with
  | None -> failwith @@ Format.asprintf "unbound %s indice (simplify) %s" x ind
  | Some i -> i

let mk_module registered_modules m =
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

  let aux = function
    | Ref_func i -> Ref_func (map_symb_raw find_func i)
    | Global_get i -> Global_get (map_symb_raw find_global i)
    | (I64_const _ | F32_const _ | F64_const _ | Ref_null _ | I32_const _) as c
      ->
      c
    | i -> failwith @@ Format.asprintf "TODO expr: `%a`" Pp.instr i
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
          let indice = map_symb find_func indice in
          { env with start = Some indice }
        | MFunc f ->
          let curr_func = env.curr_func + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_funcs id curr_func) f.id;
          let funcs = Local f :: env.funcs in
          { env with curr_func; funcs }
        | MGlobal g ->
          let curr_global = env.curr_global + 1 in
          Option.iter (fun id -> Hashtbl.add seen_globals id curr_global) g.id;
          let globals_tmp = Local (g.type_, g.init) :: env.globals_tmp in
          { env with curr_global; globals_tmp }
        | MExport _ -> env
        | MImport { desc = Import_func (id, _t); module_; name } ->
          let curr_func = env.curr_func + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_funcs id curr_func) id;
          let module_indice = find_module module_ in
          let funcs = Imported (module_indice, Symbolic name) :: env.funcs in
          { env with curr_func; funcs }
        | MImport { desc = Import_mem (id, _t); module_; name } ->
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          let module_indice = find_module module_ in
          let memories =
            Imported (module_indice, Symbolic name) :: env.memories
          in
          { env with curr_memory; memories }
        | MImport { desc = Import_table (id, _t); module_; name } ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_tables id curr_table) id;
          let module_indice = find_module module_ in
          let tables = Imported (module_indice, Symbolic name) :: env.tables in
          { env with curr_table; tables }
        | MImport { desc = Import_global (id, _t); module_; name } ->
          let curr_global = env.curr_global + 1 in
          Option.iter (fun id -> Hashtbl.add seen_globals id curr_global) id;
          let module_indice = find_module module_ in
          let globals_tmp =
            Imported (module_indice, Symbolic name) :: env.globals_tmp
          in
          { env with curr_global; globals_tmp }
        | MMem (id, { min; max }) ->
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          let new_bytes = Bytes.make (min * page_size) '\000' in
          let memories = Local (new_bytes, max) :: env.memories in
          { env with curr_memory; memories }
        | MTable (id, ({ min; max }, rt)) ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.add seen_tables id curr_table) id;
          let a = Array.make min None in
          let tbl = Local (rt, a, max) in
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
            | Data_active (_indice, _expr) -> ""
          in
          let datas = data :: env.datas in
          { env with datas; curr_data } )
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
      ; globals_tmp = []
      ; memories = []
      ; tables = []
      ; types = []
      ; start = None
      }
      m.Types.fields
  in

  let globals_tmp =
    List.map
      (function
        | Local (gt, b) -> Local (gt, List.map aux b) | Imported _ as i -> i )
      env.globals_tmp
  in

  let start = env.start in
  let datas = List.rev env.datas |> Array.of_list in
  let globals_tmp = List.rev globals_tmp |> Array.of_list in
  let tables = List.rev env.tables |> Array.of_list in
  let memories = List.rev env.memories |> Array.of_list in
  let funcs = List.rev env.funcs |> Array.of_list in

  let fields =
    List.fold_left
      (fun fields -> function
        | MExport { name; desc } ->
          let desc =
            match desc with
            | Export_func indice ->
              Export_func
                (Option.map
                   (fun indice -> map_symb_raw find_func indice)
                   indice )
            | Export_table indice ->
              Export_table
                (Option.map
                   (fun indice -> map_symb_raw find_table indice)
                   indice )
            | Export_global indice ->
              Export_global
                (Option.map
                   (fun indice -> map_symb_raw find_global indice)
                   indice )
            | Export_mem indice ->
              Export_mem
                (Option.map
                   (fun indice -> map_symb_raw find_memory indice)
                   indice )
          in
          let f = MExport { name; desc } in
          f :: fields
        | MData data as f -> begin
          match data.mode with
          | Data_passive -> f :: fields
          | Data_active (indice, expr) ->
            let indice = Option.map (map_symb_raw find_memory) indice in
            let expr = List.map aux expr in
            let mode = Data_active (indice, expr) in
            let f = MData { data with mode } in
            f :: fields
        end
        | MElem e ->
          let aux = List.map aux in
          let init = List.map aux e.init in
          let mode =
            match e.mode with
            | Elem_passive -> e.mode
            | Elem_active (ti, offset) ->
              let ti = Option.map (map_symb_raw find_table) ti in
              Elem_active (ti, aux offset)
            | Elem_declarative -> e.mode
          in
          MElem { e with mode; init } :: fields
        | f -> f :: fields )
      [] m.Types.fields
  in
  let fields = List.rev fields in

  (* adding implicit type definitions *)
  let types =
    let types =
      Array.fold_left
        (fun types -> function
          | Local f -> begin
            match f.type_f with
            | Bt_ind _ind -> types
            | Bt_raw (_type_use, t) ->
              if List.mem t types then types else t :: types
          end
          | Imported _ -> types )
        env.types funcs
    in
    Array.of_list (List.rev types)
  in

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
          let pt, rt =
            match f.type_f with
            | Bt_ind ind -> types.(find_type ind)
            | Bt_raw (type_use, t) ->
              begin
                match type_use with
                | None -> ()
                | Some ind ->
                  let t' = types.(find_type ind) in
                  assert (t = t')
              end;
              t
          in

          (* adding params and locals to the locals table *)
          let locals = pt @ f.locals in
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
            if !pos = -1 then failwith @@ Format.sprintf "unbound label %s" id;
            !pos
          in

          let bt_to_raw =
            Option.map (function
              | Bt_ind ind -> Bt_raw (Some ind, types.(find_type ind))
              | Bt_raw (type_use, t) ->
                begin
                  match type_use with
                  | None -> ()
                  | Some ind ->
                    (* we check that the explicit type match the type_use, we have to remove parameters names to do so *)
                    let pt, rt = types.(find_type ind) in
                    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
                    let rt = List.map (fun t -> t) rt in
                    let t' = (pt, rt) in
                    let ok = t = t' in
                    if not ok then failwith "inline function type"
                end;
                Bt_raw (None, t) )
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
            | Local_set id -> Local_set (map_symb_raw find_local id)
            | Local_get id -> Local_get (map_symb_raw find_local id)
            | Local_tee id -> Local_tee (map_symb_raw find_local id)
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
              Call_indirect (map_symb_raw find_table tbl_i, bt)
            | Global_set id -> Global_set (map_symb_raw find_global id)
            | Global_get id -> Global_get (map_symb_raw find_global id)
            | Ref_func id -> Ref_func (map_symb_raw find_func id)
            | Table_size id -> Table_size (map_symb_raw find_table id)
            | Table_get id -> Table_get (map_symb_raw find_table id)
            | Table_set id -> Table_set (map_symb_raw find_table id)
            | Table_grow id -> Table_grow (map_symb_raw find_table id)
            | Table_init (i, i') ->
              Table_init
                (map_symb_raw find_table i, map_symb_raw find_element i')
            | Table_fill id -> Table_fill (map_symb_raw find_table id)
            | Table_copy (i, i') ->
              Table_copy (map_symb_raw find_table i, map_symb_raw find_table i')
            | Memory_init id -> Memory_init (Raw (map_symb find_data id))
            | Data_drop id -> Data_drop (Raw (map_symb find_data id))
            | Elem_drop id -> Elem_drop (Raw (map_symb find_element id))
            | ( I_unop _ | I_binop _ | I_testop _ | I_relop _ | F_unop _
              | F_load _ | F_relop _ | I32_wrap_i64 | I_load16 _ | I64_load32 _
              | Ref_null _ | Memory_copy | Memory_fill | F_reinterpret_i _
              | I_reinterpret_f _ | I64_extend_i32 _ | I64_extend32_s
              | F32_demote_f64 | I_extend8_s _ | I_extend16_s _
              | F64_promote_f32 | F_convert_i _ | I_load _ | I_load8 _
              | I_trunc_f _ | I64_store32 _ | I_trunc_sat_f _ | F_store _
              | I_store _ | Memory_size | I_store8 _ | I_store16 _ | Ref_is_null
              | F_binop _ | F32_const _ | F64_const _ | I32_const _
              | I64_const _ | Unreachable | Br _ | Br_if _ | Drop | Select _
              | Nop | Return | Memory_grow ) as i ->
              i
          and expr e block_ids = List.map (body block_ids) e in
          let body = expr f.body [] in
          Local { f with body; type_f = Bt_raw (None, (pt, rt)) }
        | Imported _ as f -> f )
      funcs
  in

  { fields
  ; funcs
  ; seen_funcs = Hashtbl.create 512
  ; memories
  ; tables
  ; types
  ; globals = [||]
  ; globals_tmp
  ; elements = [||]
  ; start
  ; datas
  ; exported_funcs
  ; exported_globals
  ; exported_memories
  ; exported_tables
  ; should_trap = None
  }

let assert_ curr_module last_module seen_modules =
  let action = action last_module seen_modules in
  function
  | Assert_return (a, res) -> (curr_module, SAssert_return (action a, res))
  | Assert_trap (a, failure) -> (curr_module, SAssert_trap (action a, failure))
  | Assert_trap_module _ ->
    (* This should have been handled before and turned into a module with `should_trap` set ! *)
    assert false
  | Assert_exhaustion (a, failure) ->
    (curr_module, SAssert_exhaustion (action a, failure))
  | Assert_malformed (module_, failure) ->
    (curr_module, SAssert_malformed (module_, failure))
  | Assert_malformed_quote _ ->
    (* This should have been checked before and removed ! *)
    assert false
  | Assert_malformed_binary (m, failure) ->
    (curr_module, SAssert_malformed_binary (m, failure))
  | Assert_invalid (module_, failure) ->
    (curr_module, SAssert_invalid (module_, failure))
  | Assert_invalid_quote (m, failure) ->
    (curr_module, SAssert_invalid_quote (m, failure))
  | Assert_invalid_binary (m, failure) ->
    (curr_module, SAssert_invalid_binary (m, failure))
  | Assert_unlinkable (m, s) -> (curr_module, SAssert_unlinkable (m, s))

let rec script scr =
  let scr =
    Module
      { id = Some "spectest"
      ; fields =
          [ MMem (Some "memory", { min = 1; max = Some 2 })
          ; MFunc
              { type_f = Bt_raw (None, ([ (None, Num_type I32) ], []))
              ; locals = []
              ; body = []
              ; id = Some "print"
              }
          ; MFunc
              { type_f = Bt_raw (None, ([ (None, Num_type I32) ], []))
              ; locals = []
              ; body = []
              ; id = Some "print_i32"
              }
          ; MFunc
              { type_f = Bt_raw (None, ([ (None, Num_type I64) ], []))
              ; locals = []
              ; body = []
              ; id = Some "print_i64"
              }
          ; MFunc
              { type_f = Bt_raw (None, ([ (None, Num_type F32) ], []))
              ; locals = []
              ; body = []
              ; id = Some "print_f32"
              }
          ; MFunc
              { type_f = Bt_raw (None, ([ (None, Num_type F64) ], []))
              ; locals = []
              ; body = []
              ; id = Some "print_f64"
              }
          ; MFunc
              { type_f =
                  Bt_raw
                    (None, ([ (None, Num_type I32); (None, Num_type F32) ], []))
              ; locals = []
              ; body = []
              ; id = Some "print_i32_f32"
              }
          ; MFunc
              { type_f =
                  Bt_raw
                    (None, ([ (None, Num_type F64); (None, Num_type F64) ], []))
              ; locals = []
              ; body = []
              ; id = Some "print_f64_f64"
              }
          ; MTable (Some "table", ({ min = 10; max = Some 20 }, Func_ref))
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
    :: scr
  in

  let _curr_module, modules, scr =
    let seen_modules = Hashtbl.create 512 in
    let registered_modules = Hashtbl.create 512 in
    List.fold_left
      (fun (curr_module, modules, scr) -> function
        | Module m ->
          let curr_module = curr_module + 1 in
          Debug.debug Format.err_formatter "simplifying module %d@." curr_module;
          Option.iter
            (fun id -> Hashtbl.replace seen_modules id curr_module)
            m.id;
          let cmd = Module_indice curr_module in
          let modules = mk_module registered_modules m :: modules in
          (curr_module, modules, cmd :: scr)
        | Assert (Assert_trap_module (m, msg)) ->
          let curr_module = curr_module + 1 in
          Debug.debug Format.err_formatter "simplifying module %d@." curr_module;
          Option.iter
            (fun id -> Hashtbl.replace seen_modules id curr_module)
            m.id;
          let cmd = Module_indice curr_module in
          let module_ = mk_module registered_modules m in
          let module_ = { module_ with should_trap = Some msg } in
          (curr_module, module_ :: modules, cmd :: scr)
        | Assert (Assert_malformed_quote (m, msg)) ->
          ( try
              match Parse.from_string (String.concat "\n" m) with
              | Ok scr -> (
                try
                  Check.script scr;
                  let _script, _modules = script scr in
                  Format.eprintf "expected: `%s`@." msg;
                  Format.eprintf "got     : Ok@.";
                  assert false
                with Failure e -> assert (e = msg) )
              | Error e ->
                let ok = e = msg in
                if not ok then begin
                  Format.eprintf "expected: `%s`@." msg;
                  Format.eprintf "got     : `%s`@." e;
                  assert false
                end
            with Failure s -> assert (s = msg) );
          (curr_module, modules, scr)
        | Assert a ->
          let curr_module, cmd =
            assert_ curr_module (Some curr_module) seen_modules a
          in
          let cmd = Assert cmd in
          (curr_module, modules, cmd :: scr)
        | Register (name, mod_name) ->
          let indice = find_module mod_name (Some curr_module) seen_modules in
          Hashtbl.replace registered_modules name indice;
          let cmd = Register_indice (name, indice) in
          (curr_module, modules, cmd :: scr)
        | Action a ->
          let cmd = Action (action (Some curr_module) seen_modules a) in
          (curr_module, modules, cmd :: scr) )
      (-1, [], []) scr
  in

  let script = List.rev scr in
  let modules = List.rev modules |> Array.of_list in

  (script, modules)
