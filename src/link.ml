open Types
open Simplify

let rec set_table (modules : module_ array) mi i new_table =
  let tables = modules.(mi).tables in
  match tables.(i) with
  | Local (rt, _old_table, max) -> tables.(i) <- Local (rt, new_table, max)
  | Imported (mi, Raw i, ()) -> set_table modules mi i new_table
  | _ -> assert false

let rec get_table (modules : module_ array) mi i =
  let tables = modules.(mi).tables in
  match tables.(i) with
  | Local (rt, tbl, max) -> ((mi, i), rt, tbl, max)
  | Imported (mi, Raw i, ()) -> get_table modules mi i
  | _ -> assert false

let rec set_global (modules : module_ array) mi i new_global =
  let globals = modules.(mi).globals in
  match globals.(i) with
  | Local (t, _old_global) -> globals.(i) <- Local (t, new_global)
  | Imported (mi, Raw i, _t) -> set_global modules mi i new_global
  | _ -> assert false

let rec get_global (modules : module_ array) mi i =
  let globals = modules.(mi).globals in
  match globals.(i) with
  | Local (gt, g) -> (mi, gt, g)
  | Imported (mi, Raw i, _t) -> get_global modules mi i
  | _ -> assert false

let rec get_func (modules : module_ array) mi i =
  let funcs = modules.(mi).funcs in
  match funcs.(i) with
  | Local f -> (mi, f)
  | Imported (m, Raw i, ()) -> get_func modules m i
  | _ -> assert false

let rec set_memory (modules : module_ array) mi i new_mem =
  let memories = modules.(mi).memories in
  match memories.(i) with
  | Local (_old_mem, max) -> memories.(i) <- Local (new_mem, max)
  | Imported (mi, Raw i, ()) -> set_memory modules mi i new_mem
  | _ -> assert false

let rec get_memory (modules : module_ array) mi i =
  let memories = modules.(mi).memories in
  match memories.(i) with
  | Local (m, max) -> (m, max)
  | Imported (mi, Raw i, ()) -> get_memory modules mi i
  | _ -> assert false

let indice_to_int = function
  | Raw i -> i
  | Symbolic id ->
    failwith
    @@ Format.sprintf
         "interpreter internal error (indice_to_int init): unbound id $%s" id

let module_ _registered_modules modules module_indice =
  Debug.debug Format.err_formatter "linking module %d/%d@." module_indice
    (Array.length modules - 1);

  let m = modules.(module_indice) in

  Debug.debug Format.err_formatter "module fetched@.";

  let funcs =
    Array.map
      (function
        | Imported (mi, Symbolic name, ()) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_funcs name with
            | None -> failwith "unknown import"
            | Some i -> i
          in
          Imported (mi, Raw i, ())
        | (Local _ | Imported _) as f -> f )
      m.funcs
  in

  let m = { m with funcs } in
  modules.(module_indice) <- m;

  Debug.debug Format.err_formatter "module updated with funcs@.";

  let memories =
    Array.map
      (function
        | Imported (mi, Symbolic name, ()) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_memories name with
            | None -> failwith "unknown import"
            | Some i -> i
          in
          Imported (mi, Raw i, ())
        | (Local _ | Imported _) as f -> f )
      m.memories
  in

  let m = { m with memories } in
  modules.(module_indice) <- m;

  Debug.debug Format.err_formatter "module updated with memories@.";

  let tables =
    Array.map
      (function
        | Imported (mi, Symbolic name, ()) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_tables name with
            | None -> failwith "unknown import"
            | Some i -> i
          in
          Imported (mi, Raw i, ())
        | (Local _ | Imported _) as f -> f )
      m.tables
  in

  let m = { m with tables } in
  modules.(module_indice) <- m;

  Debug.debug Format.err_formatter "module updated with tables@.";

  Debug.debug Format.err_formatter "MODULE 0 has %d exported globals@."
    (Hashtbl.length modules.(0).exported_globals);

  let globals =
    Array.map
      (function
        | Imported (mi, Symbolic name, t) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_globals name with
            | None -> failwith "unknown import"
            | Some i -> i
          in
          Imported (mi, Raw i, t)
        | (Local _ | Imported _) as f -> f )
      m.globals_tmp
  in

  let m = { m with globals_tmp = [||] } in
  modules.(module_indice) <- m;

  Debug.debug Format.err_formatter "module updated with globals@.";

  let global_done = ref ~-1 in

  let rec const_expr = function
    | [ I32_const n ] -> Const_I32 n
    | [ I64_const n ] -> Const_I64 n
    | [ F32_const f ] -> Const_F32 f
    | [ F64_const f ] -> Const_F64 f
    | [ Ref_null rt ] -> Const_null rt
    | [ Global_get i ] -> begin
      let i = indice_to_int i in
      if i > !global_done then failwith "unknown global";
      match globals.(i) with
      | Local (_gt, e) -> const_expr e
      | Imported (mi, i, _t) ->
        let _mi, _gt, e = get_global modules mi (indice_to_int i) in
        e
    end
    | [ Ref_func ind ] -> Const_host (indice_to_int ind)
    | e ->
      failwith @@ Format.asprintf "invalid constant expression: `%a`" Pp.expr e
  in

  let globals =
    Array.mapi
      (fun i g ->
        let res =
          match g with
          | Local (gt, e) -> Local (gt, const_expr e)
          | Imported (mi, i, t) -> Imported (mi, i, t)
        in
        global_done := i;
        res )
      globals
  in

  let m = { m with globals } in
  modules.(module_indice) <- m;

  Debug.debug Format.err_formatter "module updated with globals bis@.";

  let const_expr_to_int e =
    match const_expr e with
    | Const_I32 n -> Int32.to_int n
    | instr ->
      failwith
      @@ Format.asprintf
           "invalid constant expression, expected i32.const but got `%a`"
           Pp.const instr
  in

  let elements, _curr_func, _curr_global, _curr_memory, _curr_data, _curr_table
      =
    List.fold_left
      (fun (elems, curr_func, curr_global, curr_memory, curr_data, curr_table)
           field ->
        match field with
        | MFunc _ ->
          (elems, curr_func + 1, curr_global, curr_memory, curr_data, curr_table)
        | MExport { desc; name } ->
          begin
            match desc with
            | Export_func ind ->
              let i =
                indice_to_int @@ Option.value ind ~default:(Raw curr_func)
              in
              if Array.length funcs <= i then failwith "unknown function";
              Hashtbl.add m.exported_funcs name i
            | Export_table ind ->
              let i =
                indice_to_int @@ Option.value ind ~default:(Raw curr_table)
              in
              if Array.length tables <= i then failwith "unknown table";
              Hashtbl.add m.exported_tables name i
            | Export_global ind ->
              let i =
                indice_to_int @@ Option.value ind ~default:(Raw curr_global)
              in
              if Array.length globals <= i then failwith "unknown global";
              Hashtbl.add m.exported_globals name i
            | Export_mem ind ->
              let i =
                indice_to_int @@ Option.value ind ~default:(Raw curr_memory)
              in
              if Array.length memories <= i then failwith "unknown memory";
              Hashtbl.add m.exported_memories name i
          end;
          (elems, curr_func, curr_global, curr_memory, curr_data, curr_table)
        | MMem _ ->
          (elems, curr_func, curr_global, curr_memory + 1, curr_data, curr_table)
        | MTable _ ->
          (elems, curr_func, curr_global, curr_memory, curr_data, curr_table + 1)
        | MData data ->
          let curr_data = curr_data + 1 in
          begin
            match data.mode with
            | Data_passive -> ()
            | Data_active (indice, expr) -> (
              let indice =
                indice_to_int (Option.value indice ~default:(Raw curr_memory))
              in
              let offset =
                try const_expr_to_int expr
                with Invalid_argument _ ->
                  failwith "out of bounds memory access"
              in
              let mem_bytes, _max =
                try get_memory modules module_indice indice
                with Invalid_argument _ -> failwith "unknown memory"
              in
              let len = String.length data.init in
              try Bytes.blit_string data.init 0 mem_bytes offset len
              with Invalid_argument _ ->
                raise @@ Trap "out of bounds memory access" )
          end;
          (elems, curr_func, curr_global, curr_memory, curr_data, curr_table)
        | MElem e ->
          let init =
            Array.of_list
              ( List.flatten
              @@ List.map (List.map (fun instr -> const_expr [ instr ])) e.init
              )
          in
          let init =
            match e.mode with
            | Elem_active (ti, offset) ->
              let ti = Option.value ti ~default:(Raw curr_table) in
              let _mi, table_ref_type, table, _max =
                try get_table modules module_indice (indice_to_int ti)
                with Invalid_argument _ -> failwith "unknown table"
              in
              let offset =
                try const_expr_to_int offset
                with Invalid_argument _ -> failwith "unknown table"
              in
              if table_ref_type <> e.type_ then failwith "invalid elem type";
              if Array.length table < Array.length init + offset then
                raise @@ Trap "out of bounds table access";
              begin
                try
                  Array.iteri
                    (fun i init ->
                      table.(offset + i) <- Some (module_indice, init) )
                    init
                with Invalid_argument _ ->
                  raise @@ Trap "out of bounds table access"
              end;
              [||]
            | Elem_declarative -> [||]
            | Elem_passive -> init
          in
          ( (e.type_, init) :: elems
          , curr_func
          , curr_global
          , curr_memory
          , curr_data
          , curr_table )
        | MType _ | MStart _ ->
          (elems, curr_func, curr_global, curr_memory, curr_data, curr_table)
        | MGlobal _ ->
          (elems, curr_func, curr_global + 1, curr_memory, curr_data, curr_table)
        | MImport i -> begin
          match i.desc with
          | Import_func _ ->
            ( elems
            , curr_func + 1
            , curr_global
            , curr_memory
            , curr_data
            , curr_table )
          | Import_global _ ->
            ( elems
            , curr_func
            , curr_global + 1
            , curr_memory
            , curr_data
            , curr_table )
          | Import_mem _ ->
            ( elems
            , curr_func
            , curr_global
            , curr_memory + 1
            , curr_data
            , curr_table )
          | Import_table _ ->
            ( elems
            , curr_func
            , curr_global
            , curr_memory
            , curr_data
            , curr_table + 1 )
        end )
      ([], -1, -1, -1, -1, -1) m.fields
  in

  let elements = Array.of_list @@ List.rev elements in

  let m = { m with elements } in
  modules.(module_indice) <- m;

  Debug.debug Format.err_formatter "module updated with elements@.";

  Option.iter
    (fun n ->
      let _module_indice, func =
        try get_func modules module_indice n
        with Invalid_argument _ -> failwith "start function"
      in
      let t =
        match func.type_f with
        | Bt_ind _n -> assert false
        | Bt_raw (_n, ft) -> ft
      in
      match t with [], [] -> () | _pt, _rt -> failwith "start function" )
    m.start;

  Debug.debug Format.err_formatter "module fully linked@."
