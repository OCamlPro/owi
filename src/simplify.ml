open Types

type module_ =
  { fields : module_field list
  ; funcs : func Array.t
  ; seen_funcs : (string, int) Hashtbl.t
  ; exported_funcs : (string, int) Hashtbl.t
  ; memories : (Bytes.t ref * int option) array
  ; tables : (ref_type * instr option Array.t * int option) array
  ; types : func_type Array.t
  ; globals : (global_type * expr) Array.t
  }

type action =
  | Invoke_indice of int * string * const list
  | Get of string option * string

type assert_ =
  | SAssert_return of action * result list
  | SAssert_trap of action * failure
  | SAssert_malformed of Types.module_ * failure
  | SAssert_malformed_quote of string list * failure
  | SAssert_malformed_binary of string list * failure
  | SAssert_invalid of Types.module_ * failure
  | SAssert_invalid_quote of string list * failure
  | SAssert_invalid_binary of string list * failure

type cmd =
  | Module_indice of int
  | Assert of assert_
  | Register_indice of string * int
  | Action of action

type script = module_ Array.t * cmd list

let action last_module seen_modules = function
  | Invoke (mod_name, f, args) ->
    let i =
      match mod_name with
      | None -> begin
        match last_module with
        | None -> failwith "no module defined"
        | Some i -> i
      end
      | Some mod_name -> begin
        match Hashtbl.find_opt seen_modules mod_name with
        | None -> failwith @@ Format.sprintf "unknown module $%s" mod_name
        | Some i -> i
      end
    in
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

let mk_module m =
  let fields = m.Types.fields in

  let curr_func = ref (-1) in
  let seen_funcs = Hashtbl.create 512 in
  let exported_funcs = Hashtbl.create 512 in

  let seen_memories = Hashtbl.create 512 in
  let curr_memory = ref (-1) in
  let mem_max_size = ref None in
  let mem_bytes = ref (Bytes.create 0) in

  let curr_table = ref (-1) in
  let tables = ref [] in
  let seen_tables = Hashtbl.create 512 in

  let types = Hashtbl.create 512 in
  let curr_type = ref (-1) in
  let seen_types = Hashtbl.create 512 in

  let passive_elements = ref [] in

  let curr_global = ref (-1) in
  let globals = ref [] in
  let seen_globals = Hashtbl.create 512 in

  List.iter
    (function
      | MFunc f ->
        incr curr_func;
        let i = !curr_func in
        Option.iter (fun id -> Hashtbl.replace seen_funcs id i) f.id
      | MGlobal g ->
        incr curr_global;
        Option.iter (fun id -> Hashtbl.add seen_globals id !curr_global) g.id;
        globals := (g.type_, g.init) :: !globals
      | MExport { name; desc } -> begin
        match desc with
        | Export_func indice ->
          let i =
            match indice with
            | Raw i -> Unsigned.UInt32.to_int i
            | Symbolic id -> begin
              match Hashtbl.find_opt seen_funcs id with
              | None ->
                (* TODO: removes this from the parser and add some anonymous case instead *)
                if id = "TODO_func" then
                  !curr_func
                else
                  failwith @@ Format.sprintf "undefined export %s" id
              | Some i -> i
            end
          in
          Hashtbl.replace exported_funcs name i
        | _ -> ()
      end
      | MMem (id, { min; max }) ->
        incr curr_memory;
        Option.iter (fun id -> Hashtbl.add seen_memories id !curr_memory) id;
        mem_max_size := Option.map Unsigned.UInt32.to_int max;
        mem_bytes := Bytes.create (Unsigned.UInt32.to_int min * page_size)
      | MTable (id, ({ min; max }, rt)) ->
        incr curr_table;
        Option.iter (fun id -> Hashtbl.add seen_tables id !curr_table) id;
        let tbl =
          ( rt
          , Array.make (Unsigned.UInt32.to_int min) None
          , Option.map Unsigned.UInt32.to_int max )
        in
        tables := tbl :: !tables
      | MType (id, t) -> (
        incr curr_type;
        Hashtbl.add types !curr_type t;
        match id with
        | None -> () (* TODO: is there really nothing to do ? *)
        | Some id -> Hashtbl.add seen_types id !curr_type )
      | _ -> () )
    fields;

  let tables =
    Array.of_list
    @@ List.rev_map
         (fun (rt, table, max) ->
           let table =
             Array.map
               (function
                 | None -> None
                 | Some e -> (
                   match e with
                   | Ref_func (Raw _i) as e -> Some e
                   | Ref_func (Symbolic id) -> begin
                     match Hashtbl.find_opt seen_funcs id with
                     | None -> failwith @@ Format.sprintf "unbound id %s" id
                     | Some i -> Some (Ref_func (Raw (Unsigned.UInt32.of_int i)))
                   end
                   | e -> Some e ) )
               table
           in
           (rt, table, max) )
         !tables
  in

  let curr_table = ref (-1) in

  let data_passive = ref [] in

  List.iter
    (function
      | MData data -> begin
        match data.mode with
        | Data_passive ->
          (* A passive data segment’s contents can be copied into a memory using the memory.init instruction. *)
          data_passive := data.init :: !data_passive
        | Data_active (indice, expr) -> (
          (* An active data segment copies its contents into a memory during instantiation, as specified by a memory index and a constant expression defining an offset into that memory. *)
          let indice =
            match indice with
            | Raw i -> Unsigned.UInt32.to_int i
            | Symbolic i -> (
              match Hashtbl.find_opt seen_memories i with
              | None -> failwith @@ Format.sprintf "unbound memory indice $%s" i
              | Some i -> i )
          in
          if indice <> 0 then failwith "multiple memories are not supported yet";
          let offset =
            match expr with
            | [] -> failwith "empty data offset"
            | [ I32_const n ] -> Int32.to_int n
            | _ -> failwith "TODO data offset"
          in
          let mem_bytes = !mem_bytes in
          let len = String.length data.init in
          let src = data.init in
          Debug.debug Format.std_formatter
            "blitting: src = `%s`; pos = `%d` ; mem_bytes = `%s` ; offset = \
             `%d` ; len = `%d`@."
            src 0
            (Bytes.to_string mem_bytes)
            offset len;
          try Bytes.blit_string src 0 mem_bytes offset len with
          | Invalid_argument _ -> (* TODO *) () )
      end
      | MTable _t -> incr curr_table
      | MElem e -> begin
        match e.mode with
        | Elem_passive ->
          (* A passive element segment’s elements can be copied to a table using the table.init instruction. *)
          passive_elements := (e.type_, e.init) :: !passive_elements
        | Elem_active (indice, offset) ->
          (* An active element segment copies its elements into a table during instantiation, as specified by a table index and a constant expression defining an offset into that table. *)
          let (table_ref_type, table, table_max_size), table_indice =
            let indice =
              match indice with
              | Raw indice -> Unsigned.UInt32.to_int indice
              | Symbolic id -> (
                if id = "TODO_table" then begin
                  Debug.debug Format.std_formatter "this may fail...@.";
                  (* TODO ? *)
                  max !curr_table 0
                end else
                  match Hashtbl.find_opt seen_tables id with
                  | None ->
                    failwith
                    @@ Format.sprintf "unbound table id (in elem): `%s`" id
                  | Some indice -> indice )
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
                      let rf =
                        match rf with
                        | Raw i -> i
                        | Symbolic rf -> (
                          match Hashtbl.find_opt seen_funcs rf with
                          | None ->
                            failwith @@ Format.sprintf "unbound func %s@." rf
                          | Some rf -> Unsigned.UInt32.of_int rf )
                      in
                      Some (Ref_func (Raw rf))
                    | (I32_const _ | Ref_null _) as ins -> Some ins
                    | i ->
                      failwith
                      @@ Format.asprintf "TODO element expr: `%a`" Pp.instr i
                  in
                  let pos = offset + i + j in
                  let len = Array.length table in
                  if pos >= len then (
                    let new_table = Array.make (pos + 1) None in
                    for i = 0 to len - 1 do
                      new_table.(i) <- table.(i)
                    done;
                    new_table.(pos) <- new_elem;
                    tables.(table_indice) <-
                      (table_ref_type, new_table, table_max_size)
                  ) else
                    table.(pos) <- new_elem )
                expr )
            e.init
        | Elem_declarative ->
          (* A declarative element segment is not available at runtime but merely serves to forward-declare references that are formed in code with instructions like ref.func. *)
          ()
      end
      | _ -> () )
    fields;

  let funcs =
    Array.of_list @@ List.rev
    @@ List.fold_left
         (fun acc -> function
           | MFunc f -> f :: acc
           | _field -> acc )
         [] fields
  in

  let funcs =
    Array.map
      (fun f ->
        let local_tbl = Hashtbl.create 512 in
        let type_f =
          match f.type_f with
          | FTId i -> (
            let i =
              match i with
              | Raw i -> Unsigned.UInt32.to_int i
              | Symbolic i -> (
                match Hashtbl.find_opt seen_types i with
                | None -> failwith @@ Format.sprintf "unbound type indice $%s" i
                | Some i -> i )
            in
            match Hashtbl.find_opt types i with
            | None -> failwith @@ Format.sprintf "unbound type indice %d" i
            | Some t -> FTFt t )
          | FTFt _ as t -> t
        in
        let param_n =
          match type_f with
          | FTId _i -> failwith "TODO FTId (simplify)"
          | FTFt (pt, _rt) ->
            List.iteri
              (fun i p ->
                match p with
                | None, _vt -> ()
                | Some id, _vt -> Hashtbl.add local_tbl id i )
              pt;
            List.length pt
        in
        List.iteri
          (fun i l ->
            match l with
            | None, _vt -> ()
            | Some id, _vt -> Hashtbl.add local_tbl id (param_n + i) )
          f.locals;
        let rec body = function
          | Br_if (Symbolic id) -> Br_if (Symbolic id) (* TODO *)
          | Br (Symbolic id) -> Br (Symbolic id) (* TODO *)
          | Call (Symbolic id) -> begin
            match Hashtbl.find_opt seen_funcs id with
            | None -> failwith @@ Format.sprintf "unbound func: %s" id
            | Some i -> Call (Raw (Unsigned.UInt32.of_int i))
          end
          | Local_set (Symbolic id) -> begin
            match Hashtbl.find_opt local_tbl id with
            | None -> failwith @@ Format.sprintf "unbound local: %s" id
            | Some i -> Local_set (Raw (Unsigned.UInt32.of_int i))
          end
          | Local_get (Symbolic id) -> begin
            match Hashtbl.find_opt local_tbl id with
            | None -> failwith @@ Format.sprintf "unbound local: %s" id
            | Some i -> Local_get (Raw (Unsigned.UInt32.of_int i))
          end
          | If_else (bt, e1, e2) -> If_else (bt, expr e1, expr e2)
          | Loop (bt, e) -> Loop (bt, expr e)
          | Block (bt, e) -> Block (bt, expr e)
          | Call_indirect (tbl_i, typ_i) ->
            let typ_i =
              match typ_i with
              | FTId i -> (
                let i =
                  match i with
                  | Raw i -> Unsigned.UInt32.to_int i
                  | Symbolic i -> (
                    match Hashtbl.find_opt seen_types i with
                    | None ->
                      failwith @@ Format.sprintf "unbound type indice $%s" i
                    | Some i -> i )
                in
                match Hashtbl.find_opt types i with
                | None -> failwith @@ Format.sprintf "unbound type indice %d" i
                | Some t -> FTFt t )
              | FTFt _ as t -> t
            in
            let tbl_i =
              match tbl_i with
              | Raw i -> Raw i
              | Symbolic tbl_i -> (
                match Hashtbl.find_opt seen_tables tbl_i with
                | None ->
                  failwith @@ Format.sprintf "unbound table id $%s" tbl_i
                | Some i -> Raw (Unsigned.UInt32.of_int i) )
            in
            Call_indirect (tbl_i, typ_i)
          | Global_set (Symbolic id) -> begin
            match Hashtbl.find_opt seen_globals id with
            | None -> failwith @@ Format.sprintf "unbound global indice $%s" id
            | Some i -> Global_set (Raw (Unsigned.UInt32.of_int i))
          end
          | Global_get (Symbolic id) -> begin
            match Hashtbl.find_opt seen_globals id with
            | None -> failwith @@ Format.sprintf "unbound global indice $%s" id
            | Some i -> Global_get (Raw (Unsigned.UInt32.of_int i))
          end
          | i -> i
        and expr e = List.map body e in
        let body = expr f.body in
        { f with body; type_f } )
      funcs
  in

  let globals = List.rev !globals |> Array.of_list in
  let types = Hashtbl.to_seq_values types |> Array.of_seq in

  { fields
  ; funcs
  ; seen_funcs = Hashtbl.create 512
  ; exported_funcs
  ; memories = [| (mem_bytes, !mem_max_size) |]
  ; tables
  ; types
  ; globals
  }

let script script =
  let modules =
    Array.of_list @@ List.rev
    @@ List.fold_left
         (fun acc -> function
           | Module m -> mk_module m :: acc
           | Assert _
           | Register _
           | Action _ ->
             acc )
         [] script
  in

  let curr_module = ref (-1) in
  let last_module = ref None in
  let seen_modules = Hashtbl.create 512 in

  let script =
    List.map
      (function
        | Module m ->
          incr curr_module;
          let i = !curr_module in
          last_module := Some i;
          Option.iter (fun id -> Hashtbl.replace seen_modules id i) m.id;
          Module_indice i
        | Assert a -> Assert (assert_ !last_module seen_modules a)
        | Register (name, mod_name) ->
          let i =
            match mod_name with
            | None -> begin
              match !last_module with
              | None -> failwith "no module defined"
              | Some i -> i
            end
            | Some mod_name -> begin
              match Hashtbl.find_opt seen_modules mod_name with
              | None -> failwith @@ Format.sprintf "unknown module $%s" mod_name
              | Some i -> i
            end
          in
          Register_indice (name, i)
        | Action a -> Action (action !last_module seen_modules a) )
      script
  in

  (script, modules)
