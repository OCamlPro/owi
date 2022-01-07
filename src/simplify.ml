open Types

type module_ =
  { fields : module_field list
  ; funcs : func Array.t
  ; seen_funcs : (string, int) Hashtbl.t
  ; exported_funcs : (string, int) Hashtbl.t
  ; memories : (Bytes.t * int option) array
  ; datas : string array
  ; tables : (ref_type * const option Array.t * int option) array
  ; types : func_type Array.t
  ; globals : (global_type * const) Array.t
  ; elements : (ref_type * const Array.t) Array.t
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
  ; globals : (global_type * expr) list
  ; mem_bytes : bytes array
  ; tables : (ref_type * const option array * int option) list
  ; types : func_type list
  ; start : int option
  }

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

let mk_module _modules m =
  let exported_funcs = Hashtbl.create 512 in

  let mem_max_size = ref None in

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

  let rec const_expr globals = function
    | [ I32_const n ] -> Const_I32 n
    | [ I64_const n ] -> Const_I64 n
    | [ F32_const f ] -> Const_F32 f
    | [ F64_const f ] -> Const_F64 f
    | [ Ref_null rt ] -> Const_null rt
    | [ Global_get ind ] ->
      let (_mut, _typ), e =
        globals.(Uint32.to_int @@ map_symb find_global ind)
      in
      const_expr globals e
    | [ Ref_func ind ] -> Const_host (Uint32.to_int @@ map_symb find_func ind)
    | e -> failwith @@ Format.asprintf "TODO global expression: `%a`" Pp.expr e
  in

  let const_expr_to_int globals e =
    match const_expr globals e with
    | Const_I32 n -> Int32.to_int n
    | _whatever -> failwith "TODO const_expr_to_int"
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
          { env with curr_func }
        | MGlobal g ->
          let curr_global = env.curr_global + 1 in
          Option.iter (fun id -> Hashtbl.add seen_globals id curr_global) g.id;
          let globals = (g.type_, g.init) :: env.globals in
          { env with curr_global; globals }
        | MExport { name; desc } ->
          begin
            match desc with
            | Export_func indice ->
              let i = map_symb_opt env.curr_func find_func indice in
              Hashtbl.replace exported_funcs name i
            | _ -> ()
          end;
          env
        | MImport { desc = Import_func (id, _t); _ } ->
          let curr_func = env.curr_func + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_funcs id curr_func) id;
          { env with curr_func }
        | MImport { desc = Import_mem (id, _t); module_; name } ->
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          if module_ = "spectest" && name = "memory" then begin
            mem_max_size := Some 2;
            env.mem_bytes.(curr_memory) <- Bytes.make page_size (Char.chr 0)
          end;
          (* TODO: other cases *)
          env
        | MImport { desc = Import_table (id, t); module_; name } ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.replace seen_tables id curr_table) id;
          if module_ = "spectest" && name = "table" then () else ();
          let tables = (snd t, [||], None) :: env.tables in
          { env with curr_table; tables }
        | MImport { desc = Import_global (id, t); module_; name } ->
          let curr_global = env.curr_global + 1 in
          Option.iter (fun id -> Hashtbl.add seen_globals id curr_global) id;
          let spectest = module_ = "spectest" in
          let init =
            match name with
            | "global_i32" when spectest -> [ I32_const 666l ]
            | "global_i64" when spectest -> [ I64_const 666L ]
            | "global_f32" when spectest ->
              [ F32_const (Float32.of_float 666.) ]
            | "global_f64" when spectest ->
              [ F64_const (Float64.of_float 666.) ]
            | name ->
              failwith
              @@ Format.asprintf
                   "TODO Import_global (module = %s; name = %s; id = %a)"
                   module_ name Pp.id_opt id
          in
          let globals = (t, init) :: env.globals in
          { env with curr_global; globals }
        | MMem (id, { min; max }) ->
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          mem_max_size := Option.map Int32.to_int max;
          env.mem_bytes.(curr_memory) <-
            Bytes.make (Int32.to_int min * page_size) (Char.chr 0);
          env
        | MTable (id, ({ min; max }, rt)) ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.add seen_tables id curr_table) id;
          let a = Array.make (Int32.to_int min) None in
          (* TODO: let a =
               Array.map
                 (Option.map (function
                   | Ref_func id -> Ref_func (Raw (map_symb find_func id))
                   | e -> e ) )
                 a
             in *)
          let tbl = (rt, a, Option.map Int32.to_int max) in
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
              let indice = map_symb_opt 0 find_memory indice in
              match const_expr (List.rev env.globals |> Array.of_list) expr with
              | Const_I32 offset ->
                let mem_bytes = env.mem_bytes.(indice) in
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
      ; globals = []
      ; mem_bytes = Array.init 1 (fun _i -> Bytes.make 0 (Char.chr 0))
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
  let const_expr = const_expr globals in
  let const_expr_to_int = const_expr_to_int globals in
  let globals = Array.map (fun (type_, e) -> (type_, const_expr e)) globals in

  let _curr_table, elements =
    List.fold_left
      (fun (curr_table, elements) -> function
        | MTable _ -> (curr_table + 1, elements)
        | MElem e -> (
          match e.mode with
          | Elem_passive ->
            let new_elements =
              List.rev_map (fun expr -> (e.type_, const_expr expr)) e.init
            in
            (curr_table, new_elements @ elements)
          | Elem_active (ti, offset) ->
            let ti = map_symb_opt env.curr_table find_table ti in
            let table_ref_type, table, table_max_size = tables.(ti) in
            let offset = const_expr_to_int offset in
            if table_ref_type <> e.type_ then failwith "invalid elem type";
            List.iteri
              (fun i expr ->
                List.iteri
                  (fun j x ->
                    let new_elem = const_expr [ x ] in
                    let pos = offset + i + j in
                    let len = Array.length table in
                    if pos >= len then (
                      let new_table = Array.make (pos + 1) None in
                      Array.iteri (fun i e -> new_table.(i) <- e) table;
                      new_table.(pos) <- Some new_elem;
                      tables.(ti) <- (table_ref_type, new_table, table_max_size)
                      )
                    else table.(pos) <- Some new_elem )
                  expr )
              e.init;
            (curr_table, elements)
          | Elem_declarative -> (curr_table, elements) )
        | _ -> (curr_table, elements) )
      (-1, []) m.Types.fields
  in

  let elements = List.rev elements |> Array.of_list in
  let elements = Array.map (fun (rt, l) -> (rt, [| l |])) elements in

  let funcs =
    Array.of_list @@ List.rev
    @@ List.fold_left
         (fun acc -> function
           | MFunc f -> f :: acc
           | MImport { desc = Import_func (id, type_f); _ } ->
             (* TODO: how to import the function ? *)
             { type_f; id; locals = []; body = [] } :: acc
           | _field -> acc )
         [] m.Types.fields
  in

  let funcs =
    Array.map
      (fun f ->
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
        { f with body; type_f = Bt_raw type_f } )
      funcs
  in

  { fields = m.Types.fields
  ; funcs
  ; seen_funcs = Hashtbl.create 512
  ; exported_funcs
  ; memories = [| (env.mem_bytes.(0), !mem_max_size) |]
  ; tables
  ; types
  ; globals
  ; elements
  ; start
  ; datas
  }

let script script =
  let _curr_module, modules, script =
    let seen_modules = Hashtbl.create 512 in
    List.fold_left
      (fun (curr_module, modules, script) -> function
        | Module m ->
          let curr_module = curr_module + 1 in
          Option.iter
            (fun id -> Hashtbl.replace seen_modules id curr_module)
            m.id;
          let cmd = Module_indice curr_module in
          let modules = mk_module modules m :: modules in
          (curr_module, modules, cmd :: script)
        | Assert a ->
          let cmd = Assert (assert_ (Some curr_module) seen_modules a) in
          (curr_module, modules, cmd :: script)
        | Register (name, mod_name) ->
          let cmd =
            Register_indice
              (name, find_module mod_name (Some curr_module) seen_modules)
          in
          (curr_module, modules, cmd :: script)
        | Action a ->
          let cmd = Action (action (Some curr_module) seen_modules a) in
          (curr_module, modules, cmd :: script) )
      (-1, [], []) script
  in

  let script = List.rev script in
  let modules = List.rev modules |> Array.of_list in

  (script, modules)
