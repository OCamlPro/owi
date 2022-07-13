open Types

let check_limit min max =
  Option.iter
    (fun max ->
      if min > max then failwith "size minimum must not be greater than maximum"
      )
    max

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
  ; should_not_link : string option
  }

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
    | _i -> failwith "constant expression required"
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
        | MImport { desc = Import_func (id, t); module_; name } ->
          ( match t with
          | Bt_raw (None, _) -> ()
          | Bt_ind ind | Bt_raw (Some ind, _) -> (
            match ind with
            | Raw n -> if n > env.curr_type then failwith "unknown type"
            | Symbolic _n -> () (* TODO: check if known type*) ) );
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
          if min > 65536 then
            failwith "memory size must be at most 65536 pages (4GiB)";
          Option.iter
            (fun max ->
              if max > 65536 then
                failwith "memory size must be at most 65536 pages (4GiB)" )
            max;
          check_limit min max;
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          let new_bytes = Bytes.make (min * page_size) '\000' in
          let memories = Local (new_bytes, max) :: env.memories in
          { env with curr_memory; memories }
        | MTable (id, ({ min; max }, rt)) ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.add seen_tables id curr_table) id;
          check_limit min max;
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
        | Local (gt, b) ->
          let b = List.map aux b in
          List.iter
            (function
              | Ref_func i ->
                let i = map_symb find_func i in
                if i >= List.length env.funcs then failwith "unknown function"
              | _instr -> () )
            b;
          Local (gt, b)
        | Imported _ as i -> i )
      env.globals_tmp
  in

  let start = env.start in
  let datas = List.rev env.datas |> Array.of_list in
  let globals_tmp = List.rev globals_tmp |> Array.of_list in
  let tables = List.rev env.tables |> Array.of_list in
  let memories = List.rev env.memories |> Array.of_list in
  let funcs = List.rev env.funcs |> Array.of_list in

  let add_exported_name =
    let tbl = Hashtbl.create 512 in
    fun name ->
      if Hashtbl.mem tbl name then failwith "duplicate export name";
      Hashtbl.add tbl name ()
  in

  let fields =
    List.fold_left
      (fun fields -> function
        | MExport { name; desc } ->
          add_exported_name name;
          let desc =
            match desc with
            | Export_func indice ->
              Export_func (Option.map (map_symb_raw find_func) indice)
            | Export_table indice ->
              Export_table (Option.map (map_symb_raw find_table) indice)
            | Export_global indice ->
              Export_global (Option.map (map_symb_raw find_global) indice)
            | Export_mem indice ->
              Export_mem (Option.map (map_symb_raw find_memory) indice)
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
          List.iter
            (List.iter (function
              | Ref_func i ->
                if map_symb find_func i >= Array.length funcs then
                  failwith "unknown function"
              | _instr -> () ) )
            init;
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
            | Bt_ind ind -> begin
              try types.(find_type ind)
              with Invalid_argument _ -> failwith "unknown type"
            end
            | Bt_raw (type_use, t) -> begin
              (* TODO: move this to check ?*)
              match type_use with
              | None -> t
              | Some ind ->
                let t' =
                  try types.(find_type ind)
                  with Invalid_argument _ -> failwith "unknown type"
                in
                let func_type_equal (p1, r1) (p2, r2) =
                  (* we ignore the argument name *)
                  let p1 = List.map snd p1 in
                  let p2 = List.map snd p2 in
                  r1 = r2 && p1 = p2
                in
                if not (func_type_equal t t') then
                  failwith "inline function type";
                t' (* TODO: t ? *)
            end
          in

          (* adding params and locals to the locals table *)
          let locals = pt @ f.locals in
          List.iteri
            (fun i (id, _t) ->
              Option.iter (fun id -> Hashtbl.add local_tbl id i) id )
            locals;

          (* block_ids handling *)
          let block_id_to_raw (loop_count, block_ids) id =
            let id =
              match id with
              | Symbolic id ->
                Debug.debug Format.err_formatter "SYMBOLIC BLOCK ID %s@\n" id;
                let pos = ref (-1) in
                begin
                  try
                    List.iteri
                      (fun i n ->
                        if n = Some id then begin
                          pos := i;
                          raise Exit
                        end )
                      block_ids
                  with Exit -> ()
                end;
                if !pos = -1 then
                  failwith @@ Format.sprintf "unbound label %s" id;
                !pos
              | Raw id ->
                Debug.debug Format.err_formatter "RAW BLOCK ID %d@\n" id;
                id
            in
            (* this is > and not >= because you can `br 0` without any block to target the function *)
            if id > List.length block_ids + loop_count then
              failwith "unknown label";
            Raw id
          in

          let bt_to_raw =
            Option.map (function
              | Bt_ind ind ->
                let pt, rt =
                  try types.(find_type ind)
                  with Invalid_argument _ -> failwith "unknown type"
                in
                Bt_raw (Some ind, (pt, rt))
              | Bt_raw (type_use, t) ->
                begin
                  match type_use with
                  | None -> ()
                  | Some ind ->
                    (* TODO: move this to check ? *)
                    (* we check that the explicit type match the type_use, we have to remove parameters names to do so *)
                    let pt, rt =
                      try types.(find_type ind)
                      with Invalid_argument _ -> failwith "unknown type"
                    in
                    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
                    let t' = (pt, rt) in
                    let ok = t = t' in
                    if not ok then failwith "inline function type"
                end;
                Bt_raw (None, t) )
          in

          (* handling an expression *)
          let rec body (loop_count, block_ids) = function
            | Br_table (ids, id) ->
              let f = block_id_to_raw (loop_count, block_ids) in
              Br_table (Array.map f ids, f id)
            | Br_if id -> Br_if (block_id_to_raw (loop_count, block_ids) id)
            | Br id -> Br (block_id_to_raw (loop_count, block_ids) id)
            | Call id ->
              let id = map_symb find_func id in
              if id >= Array.length funcs then failwith "unknown function";
              Call (Raw id)
            | Local_set id -> Local_set (map_symb_raw find_local id)
            | Local_get id -> Local_get (map_symb_raw find_local id)
            | Local_tee id -> Local_tee (map_symb_raw find_local id)
            | If_else (id, bt, e1, e2) ->
              let bt = bt_to_raw bt in
              let block_ids = id :: block_ids in
              If_else
                ( id
                , bt
                , expr e1 (loop_count, block_ids)
                , expr e2 (loop_count, block_ids) )
            | Loop (id, bt, e) ->
              let bt = bt_to_raw bt in
              let e = expr e (loop_count + 1, id :: block_ids) in
              Loop (id, bt, e)
            | Block (id, bt, e) ->
              let bt = bt_to_raw bt in
              Block (id, bt, expr e (loop_count, id :: block_ids))
            | Call_indirect (tbl_i, bt) ->
              let tbl_i = map_symb find_table tbl_i in
              if tbl_i >= Array.length tables then failwith "unknown table";
              let bt = Option.get @@ bt_to_raw (Some bt) in
              Call_indirect (Raw tbl_i, bt)
            | Global_set id -> Global_set (map_symb_raw find_global id)
            | Global_get id -> Global_get (map_symb_raw find_global id)
            | Ref_func id -> Ref_func (map_symb_raw find_func id)
            | Table_size id -> Table_size (map_symb_raw find_table id)
            | Table_get id -> Table_get (map_symb_raw find_table id)
            | Table_set id -> Table_set (map_symb_raw find_table id)
            | Table_grow id -> Table_grow (map_symb_raw find_table id)
            | Table_init (i, i') ->
              let i = map_symb find_table i in
              if i >= Array.length tables then failwith "unknown table";
              (* TODO: check i' ? *)
              Table_init (Raw i, map_symb_raw find_element i')
            | Table_fill id -> Table_fill (map_symb_raw find_table id)
            | Table_copy (i, i') ->
              Table_copy (map_symb_raw find_table i, map_symb_raw find_table i')
            | Memory_init id ->
              if Array.length memories < 1 then failwith "unknown memory";
              let id = map_symb find_data id in
              if id >= Array.length datas then failwith "unknown data segment";
              Memory_init (Raw id)
            | Data_drop id ->
              let id = map_symb find_data id in
              if id >= Array.length datas then failwith "unknown data segment";
              Data_drop (Raw id)
            | Elem_drop id ->
              let id = map_symb find_element id in
              if id > env.curr_element then failwith "unknown elem segment";
              Elem_drop (Raw id)
            | ( I_load8 _ | I_load16 _ | I64_load32 _ | I_load _ | F_load _
              | I64_store32 _ | I_store8 _ | I_store16 _ | F_store _ | I_store _
              | Memory_copy | Memory_size | Memory_fill | Memory_grow ) as i ->
              if Array.length memories < 1 then failwith "unknown memory";
              i
            | ( I_unop _ | I_binop _ | I_testop _ | I_relop _ | F_unop _
              | F_relop _ | I32_wrap_i64 | Ref_null _ | F_reinterpret_i _
              | I_reinterpret_f _ | I64_extend_i32 _ | I64_extend32_s
              | F32_demote_f64 | I_extend8_s _ | I_extend16_s _
              | F64_promote_f32 | F_convert_i _ | I_trunc_f _ | I_trunc_sat_f _
              | Ref_is_null | F_binop _ | F32_const _ | F64_const _
              | I32_const _ | I64_const _ | Unreachable | Drop | Select _ | Nop
              | Return ) as i ->
              i
          and expr e (loop_count, block_ids) =
            List.map (body (loop_count, block_ids)) e
          in
          let body = expr f.body (0, []) in
          Local { f with body; type_f = Bt_raw (None, (pt, rt)) }
        | Imported _ as f -> f )
      funcs
  in

  Option.iter
    (fun i -> if i >= Array.length funcs then failwith "unknown function")
    start;

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
  ; should_not_link = None
  }
