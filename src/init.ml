open Types
open Simplify

let rec get_table (modules : module_ array) mi i =
  let tables = modules.(mi).tables in
  match tables.(i) with
  | Local (rt, tbl, max) ->
    (mi, rt, tbl, max, fun tbl -> tables.(i) <- Local (rt, tbl, max))
  | Imported (mi, Raw i) -> get_table modules mi (Uint32.to_int i)
  | _ -> failwith @@ Format.sprintf "get_table got Symbolic id"

let rec get_global (modules : module_ array) mi i =
  let globals = modules.(mi).globals in
  match globals.(i) with
  | Local (gt, g) -> (mi, gt, g, fun g -> globals.(i) <- Local (gt, g))
  | Imported (mi, Raw i) -> get_global modules mi (Uint32.to_int i)
  | _ -> failwith @@ Format.sprintf "get_global got Symbolic id"

let rec get_func (modules : module_ array) mi i =
  let funcs = modules.(mi).funcs in
  match funcs.(i) with
  (* TODO: do we need set somewhere ? *)
  | Local f -> (mi, f, fun f -> funcs.(i) <- Local f)
  | Imported (m, Raw i) -> get_func modules m (Uint32.to_int i)
  | _ -> failwith @@ Format.sprintf "get_func got Symbolic id"

let rec get_memory (modules : module_ array) mi i =
  let memories = modules.(mi).memories in
  match memories.(i) with
  | Local (m, max) -> (mi, m, max, fun m max -> memories.(i) <- Local (m, max))
  | Imported (mi, Raw i) -> get_memory modules mi (Uint32.to_int i)
  | _ -> failwith @@ Format.sprintf "get_memory got Symbolic id"

let indice_to_int = function
  | Raw i -> Uint32.to_int i
  | Symbolic id ->
    failwith
    @@ Format.sprintf
         "interpreter internal error (indice_to_int init): unbound id $%s" id

let module_ _registered_modules modules module_indice =
  let m = modules.(module_indice) in

  let funcs =
    Array.map
      (function
        | Imported (mi, Symbolic name) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_funcs name with
            | None -> failwith @@ Format.sprintf "unbound imported func %s" name
            | Some i -> Uint32.of_int i
          in
          Imported (mi, Raw i)
        | (Local _ | Imported _) as f -> f )
      m.funcs
  in

  let m = { m with funcs } in
  modules.(module_indice) <- m;

  let memories =
    Array.map
      (function
        | Imported (mi, Symbolic name) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_memories name with
            | None ->
              failwith @@ Format.sprintf "unbound imported memories %s" name
            | Some i -> Uint32.of_int i
          in
          Imported (mi, Raw i)
        | (Local _ | Imported _) as f -> f )
      m.memories
  in

  let m = { m with memories } in
  modules.(module_indice) <- m;

  let tables =
    Array.map
      (function
        | Imported (mi, Symbolic name) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_tables name with
            | None ->
              failwith @@ Format.sprintf "unbound imported tables %s" name
            | Some i -> Uint32.of_int i
          in
          Imported (mi, Raw i)
        | (Local _ | Imported _) as f -> f )
      m.tables
  in

  let m = { m with tables } in
  modules.(module_indice) <- m;

  let globals =
    Array.map
      (function
        | Imported (mi, Symbolic name) ->
          let i =
            match Hashtbl.find_opt modules.(mi).exported_globals name with
            | None ->
              failwith @@ Format.sprintf "unbound imported globals %s" name
            | Some i -> Uint32.of_int i
          in
          Imported (mi, Raw i)
        | (Local _ | Imported _) as f -> f )
      m.globals_tmp
  in

  let m = { m with globals_tmp = [||] } in
  modules.(module_indice) <- m;

  let rec const_expr = function
    | [ I32_const n ] -> Const_I32 n
    | [ I64_const n ] -> Const_I64 n
    | [ F32_const f ] -> Const_F32 f
    | [ F64_const f ] -> Const_F64 f
    | [ Ref_null rt ] -> Const_null rt
    | [ Global_get i ] -> begin
      match globals.(indice_to_int i) with
      | Local (_gt, e) -> const_expr e
      | Imported (mi, i) ->
        let _mi, _gt, e, _set = get_global modules mi (indice_to_int i) in
        e
    end
    | [ Ref_func ind ] -> Const_host (indice_to_int ind)
    | e -> failwith @@ Format.asprintf "TODO global expression: `%a`" Pp.expr e
  in

  let globals =
    Array.map
      (function
        | Local (gt, e) -> Local (gt, const_expr e)
        | Imported (mi, i) -> Imported (mi, i) )
      globals
  in

  let m = { m with globals } in
  modules.(module_indice) <- m;

  let const_expr_to_int e =
    match const_expr e with
    | Const_I32 n -> Int32.to_int n
    | _whatever -> failwith "TODO const_expr_to_int"
  in

  ignore
  @@ List.fold_left
       (fun (curr_func, curr_global, curr_memory, curr_data, curr_table) field ->
         match field with
         | MFunc _ ->
           (curr_func + 1, curr_global, curr_memory, curr_data, curr_table)
         | MExport { desc; name } ->
           begin
             match desc with
             | Export_func ind ->
               let ind =
                 Option.value ind ~default:(Raw (Uint32.of_int curr_func))
               in
               Hashtbl.add m.exported_funcs name (indice_to_int ind)
             | Export_table ind ->
               let ind =
                 Option.value ind ~default:(Raw (Uint32.of_int curr_table))
               in
               Hashtbl.add m.exported_tables name (indice_to_int ind)
             | Export_global ind ->
               let ind =
                 Option.value ind ~default:(Raw (Uint32.of_int curr_global))
               in
               Hashtbl.add m.exported_globals name (indice_to_int ind)
             | Export_mem ind ->
               let ind =
                 Option.value ind ~default:(Raw (Uint32.of_int curr_memory))
               in
               Hashtbl.add m.exported_memories name (indice_to_int ind)
           end;
           (curr_func, curr_global, curr_memory, curr_data, curr_table)
         | MMem _ ->
           (curr_func, curr_global, curr_memory + 1, curr_data, curr_table)
         | MTable _ ->
           (curr_func, curr_global, curr_memory, curr_data, curr_table + 1)
         | MData data ->
           let curr_data = curr_data + 1 in
           begin
             match data.mode with
             | Data_passive -> ()
             | Data_active (indice, expr) ->
               let indice =
                 indice_to_int
                   (Option.value indice
                      ~default:(Raw (Uint32.of_int curr_memory)) )
               in
               let offset = const_expr_to_int expr in
               let _mi, mem_bytes, _max, _set =
                 get_memory modules module_indice indice
               in
               let len = String.length data.init in
               Bytes.blit_string data.init 0 mem_bytes offset len
           end;
           (curr_func, curr_global, curr_memory, curr_data, curr_table)
         | MElem e ->
           begin
             match e.mode with
             | Elem_active (ti, offset) ->
               let ti =
                 Option.value ti ~default:(Raw (Uint32.of_int curr_table))
               in
               let _mi, table_ref_type, table, _max, set_table =
                 get_table modules module_indice (indice_to_int ti)
               in
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
                         set_table new_table )
                       else table.(pos) <- Some new_elem )
                     expr )
                 e.init
             | _ -> ()
           end;
           (curr_func, curr_global, curr_memory, curr_data, curr_table)
         | MType _ | MStart _ ->
           (curr_func, curr_global, curr_memory, curr_data, curr_table)
         | MGlobal _ ->
           (curr_func, curr_global + 1, curr_memory, curr_data, curr_table)
         | MImport i -> begin
           match i.desc with
           | Import_func _ ->
             (curr_func + 1, curr_global, curr_memory, curr_data, curr_table)
           | Import_global _ ->
             (curr_func, curr_global + 1, curr_memory, curr_data, curr_table)
           | Import_mem _ ->
             (curr_func, curr_global, curr_memory + 1, curr_data, curr_table)
           | Import_table _ ->
             (curr_func, curr_global, curr_memory, curr_data, curr_table + 1)
         end )
       (-1, -1, -1, -1, -1) m.fields
