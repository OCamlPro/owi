open Types

type module_ =
  { fields : module_field list
  ; funcs : func Array.t
  ; seen_funcs : (string, int) Hashtbl.t
  ; exported_funcs : (string, int) Hashtbl.t
  ; memories : (Bytes.t * int option) array
  ; tables : (ref_type * const option Array.t * int option) array
  ; types : func_type Array.t
  ; globals : (global_type * expr) Array.t
  ; elements : (ref_type * const Array.t) Array.t
  }

type action =
  | Invoke_indice of int * string * const list
  | Get of string option * string

type assert_ =
  | SAssert_return of action * result list
  | SAssert_trap of action * string
  | SAssert_malformed of Types.module_ * string
  | SAssert_malformed_quote of string list * string
  | SAssert_malformed_binary of string list * string
  | SAssert_invalid of Types.module_ * string
  | SAssert_invalid_quote of string list * string
  | SAssert_invalid_binary of string list * string

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
  | Some sym -> map_symb find_in_tbl sym

let find_module name last seen =
  match name with
  | None -> begin
    match last with
    | None -> failwith "no module defined"
    | Some i -> i
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
  | Get _ -> failwith "not yet implemented"

let assert_ last_module seen_modules =
  let action = action last_module seen_modules in
  function
  | Assert_return (a, res) -> SAssert_return (action a, res)
  | Assert_trap (a, failure) -> SAssert_trap (action a, failure)
  | Assert_malformed (module_, failure) -> SAssert_malformed (module_, failure)
  | Assert_malformed_quote (m, failure) -> SAssert_malformed_quote (m, failure)
  | Assert_malformed_binary (m, failure) -> SAssert_malformed_binary (m, failure)
  | Assert_invalid (module_, failure) -> SAssert_invalid (module_, failure)
  | Assert_invalid_quote (m, failure) -> SAssert_malformed_quote (m, failure)
  | Assert_invalid_binary (m, failure) -> SAssert_invalid_binary (m, failure)

type env =
  { curr_func : int
  ; curr_global : int
  ; curr_memory : int
  ; curr_table : int
  ; curr_type : int
  ; data_passive : string list
  ; globals : (global_type * expr) list
  ; mem_bytes : Bytes.t Array.t
  ; tables : (ref_type * const option array * int option) list
  }

let mk_module m =
  let seen_funcs = Hashtbl.create 512 in
  let exported_funcs = Hashtbl.create 512 in

  let seen_memories = Hashtbl.create 512 in
  let mem_max_size = ref None in

  let seen_tables = Hashtbl.create 512 in

  let types = Hashtbl.create 512 in
  let seen_types = Hashtbl.create 512 in

  let seen_globals = Hashtbl.create 512 in

  let find_func id =
    match Hashtbl.find_opt seen_funcs id with
    | None -> failwith @@ Format.sprintf "unbound func %s" id
    | Some i -> i
  in

  let find_memory id =
    match Hashtbl.find_opt seen_memories id with
    | None -> failwith @@ Format.sprintf "unbound memory %s" id
    | Some i -> i
  in

  let find_global id =
    match Hashtbl.find_opt seen_globals id with
    | None -> failwith @@ Format.sprintf "unbound global %s" id
    | Some i -> i
  in

  let find_table id =
    match Hashtbl.find_opt seen_tables id with
    | None -> failwith @@ Format.sprintf "unbound table indice $%s" id
    | Some i -> i
  in

  let env =
    List.fold_left
      (fun env -> function
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
              let i =
                Uint32.to_int
                @@ map_symb_opt (Uint32.of_int env.curr_func) find_func indice
              in
              Hashtbl.replace exported_funcs name i
            | _ -> ()
          end;
          env
        | MMem (id, { min; max }) ->
          let curr_memory = env.curr_memory + 1 in
          Option.iter (fun id -> Hashtbl.add seen_memories id curr_memory) id;
          mem_max_size := Option.map Uint32.to_int max;
          env.mem_bytes.(curr_memory) <-
            Bytes.create (Uint32.to_int min * page_size);
          env
        | MTable (id, ({ min; max }, rt)) ->
          let curr_table = env.curr_table + 1 in
          Option.iter (fun id -> Hashtbl.add seen_tables id curr_table) id;
          let a = Array.make (Uint32.to_int min) None in
          (* TODO: let a =
               Array.map
                 (Option.map (function
                   | Ref_func id -> Ref_func (Raw (map_symb find_func id))
                   | e -> e ) )
                 a
             in *)
          let tbl = (rt, a, Option.map Uint32.to_int max) in
          let tables = tbl :: env.tables in
          { env with curr_table; tables }
        | MType (id, t) ->
          let curr_type = env.curr_type + 1 in
          Hashtbl.add types curr_type t;
          ( match id with
          | None -> () (* TODO: is there really nothing to do ? *)
          | Some id -> Hashtbl.add seen_types id curr_type );
          { env with curr_type }
        | MData data -> begin
          match data.mode with
          | Data_passive ->
            (* A passive data segment’s contents can be copied into a memory using the memory.init instruction. *)
            let data_passive = data.init :: env.data_passive in
            { env with data_passive }
          | Data_active (indice, expr) ->
            (* An active data segment copies its contents into a memory during instantiation, as specified by a memory index and a constant expression defining an offset into that memory. *)
            let indice = map_symb_opt Uint32.zero find_memory indice in
            if indice <> Uint32.zero then
              failwith "multiple memories are not supported yet";
            let offset =
              match expr with
              | [] -> failwith "empty data offset"
              | [ I32_const n ] -> Int32.to_int n
              | _ -> failwith "TODO data offset"
            in
            let mem_bytes = env.mem_bytes.(Uint32.to_int indice) in
            let len = String.length data.init in
            let src = data.init in
            Debug.debug Format.std_formatter
              "blitting: src = `%s`; pos = `%d` ; mem_bytes = `%s` ; offset = \
               `%d` ; len = `%d`@."
              src 0
              (Bytes.to_string mem_bytes)
              offset len;
            begin
              try Bytes.blit_string src 0 mem_bytes offset len with
              | Invalid_argument _ -> (* TODO *) ()
            end;
            env
        end
        | _ -> env )
      { curr_func = -1
      ; curr_global = -1
      ; curr_memory = -1
      ; curr_table = -1
      ; curr_type = -1
      ; data_passive = []
      ; globals = []
      ; mem_bytes = Array.init 1 (fun _i -> Bytes.create 0)
      ; tables = []
      }
      m.Types.fields
  in

  let globals = List.rev env.globals |> Array.of_list in
  let tables = List.rev env.tables |> Array.of_list in

  let curr_table = ref (-1) in
  let passive_elements = ref [] in

  List.iter
    (function
      | MTable _t -> incr curr_table
      | MElem e -> begin
        match e.mode with
        | Elem_passive ->
          (* A passive element segment’s elements can be copied to a table using the table.init instruction. *)
          List.iter
            (fun expr ->
              let expr =
                List.map
                  (function
                    | Ref_func rf ->
                      Const_host (Uint32.to_int @@ map_symb find_func rf)
                    | I32_const n -> Const_I32 n
                    | I64_const n -> Const_I64 n
                    | F32_const f -> Const_F32 f
                    | F64_const f -> Const_F64 f
                    | Ref_null rt -> Const_null rt
                    | i ->
                      failwith
                      @@ Format.asprintf "TODO element expr: `%a`" Pp.instr i )
                  expr
              in
              passive_elements := (e.type_, expr) :: !passive_elements )
            e.init
        | Elem_active (indice, offset) ->
          (* An active element segment copies its elements into a table during instantiation, as specified by a table index and a constant expression defining an offset into that table. *)
          let (table_ref_type, table, table_max_size), table_indice =
            let indice =
              Uint32.to_int
              @@ map_symb_opt (Uint32.of_int !curr_table) find_table indice
            in
            (tables.(indice), indice)
          in
          let offset =
            match offset with
            | [] -> failwith "empty offset"
            | [ I32_const n ] -> Int32.to_int n
            | _ -> failwith "unhandled offset expr kind"
          in
          if table_ref_type <> e.type_ then failwith "invalid elem type";
          List.iteri
            (fun i expr ->
              List.iteri
                (fun j x ->
                  let new_elem =
                    match x with
                    | Ref_func rf ->
                      Const_host (Uint32.to_int @@ map_symb find_func rf)
                    | I32_const n -> Const_I32 n
                    | I64_const n -> Const_I64 n
                    | F32_const f -> Const_F32 f
                    | F64_const f -> Const_F64 f
                    | Ref_null rt -> Const_null rt
                    | i ->
                      failwith
                      @@ Format.asprintf "TODO element expr: `%a`" Pp.instr i
                  in
                  let pos = offset + i + j in
                  let len = Array.length table in
                  if pos >= len then (
                    let new_table = Array.make (pos + 1) None in
                    Array.iteri (fun i e -> new_table.(i) <- e) table;
                    new_table.(pos) <- Some new_elem;
                    tables.(table_indice) <-
                      (table_ref_type, new_table, table_max_size)
                  ) else
                    table.(pos) <- Some new_elem )
                expr )
            e.init
        | Elem_declarative ->
          (* A declarative element segment is not available at runtime but merely serves to forward-declare references that are formed in code with instructions like ref.func. *)
          ()
      end
      | _ -> () )
    m.Types.fields;

  let funcs =
    Array.of_list @@ List.rev
    @@ List.fold_left
         (fun acc -> function
           | MFunc f -> f :: acc
           | _field -> acc )
         [] m.Types.fields
  in

  let find_type ind =
    let i =
      match ind with
      | Raw i -> Uint32.to_int i
      | Symbolic i -> begin
        match Hashtbl.find_opt seen_types i with
        | None -> failwith @@ Format.asprintf "unbound type id %a" Pp.id i
        | Some i -> i
      end
    in
    match Hashtbl.find_opt types i with
    | None -> failwith @@ Format.sprintf "unbound type i %d" i
    | Some t -> t
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
          | Bt_ind ind ->
            let t = find_type ind in
            t
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
            try List.iteri (fun i n -> if n = Some id then pos := i) l with
            | Exit -> ()
          end;

          if !pos = -1 then
            failwith @@ Format.sprintf "unbound label %s" id
          else
            Uint32.of_int !pos
        in

        (* handling an expression *)
        let rec body block_ids = function
          | Br_table (ids, id) ->
            let ids =
              Array.map
                (function
                  | Symbolic id -> Raw (find_block_id id block_ids)
                  | Raw id -> Raw id )
                ids
            in
            let id =
              match id with
              | Raw id -> Raw id
              | Symbolic id -> Raw (find_block_id id block_ids)
            in
            Br_table (ids, id)
          | Br_if (Symbolic id) -> Br_if (Raw (find_block_id id block_ids))
          | Br (Symbolic id) -> Br (Raw (find_block_id id block_ids))
          | Call id -> Call (Raw (map_symb find_func id))
          | Local_set id -> Local_set (Raw (map_symb find_local id))
          | Local_get id -> Local_get (Raw (map_symb find_local id))
          | If_else (id, bt, e1, e2) ->
            let block_ids = id :: block_ids in
            If_else (id, bt, expr e1 block_ids, expr e2 block_ids)
          | Loop (id, bt, e) -> Loop (id, bt, expr e (id :: block_ids))
          | Block (id, bt, e) -> Block (id, bt, expr e (id :: block_ids))
          | Call_indirect (tbl_i, typ_i) ->
            let typ_i =
              match typ_i with
              | Bt_ind ind -> Bt_raw (find_type ind)
              | t -> t
            in
            Call_indirect (Raw (map_symb find_table tbl_i), typ_i)
          | Global_set id -> Global_set (Raw (map_symb find_global id))
          | Global_get id -> Global_get (Raw (map_symb find_global id))
          | Ref_func id -> Ref_func (Raw (map_symb find_func id))
          | Table_size id -> Table_size (Raw (map_symb find_table id))
          | Table_get id -> Table_get (Raw (map_symb find_table id))
          | Table_set id -> Table_set (Raw (map_symb find_table id))
          | Table_grow id -> Table_grow (Raw (map_symb find_table id))
          | Table_init (i, i') ->
            Table_init
              (Raw (map_symb find_table i), Raw (map_symb find_table i'))
          | i -> i
        and expr e block_ids = List.map (body block_ids) e in
        let body = expr f.body [] in
        { f with body; type_f = Bt_raw type_f } )
      funcs
  in

  let elements = List.rev !passive_elements |> Array.of_list in
  let elements = Array.map (fun (rt, l) -> (rt, Array.of_list l)) elements in
  let types = Hashtbl.to_seq_values types |> Array.of_seq in

  { fields = m.Types.fields
  ; funcs
  ; seen_funcs = Hashtbl.create 512
  ; exported_funcs
  ; memories = [| (env.mem_bytes.(0), !mem_max_size) |]
  ; tables
  ; types
  ; globals
  ; elements
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
          let modules = mk_module m :: modules in
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
