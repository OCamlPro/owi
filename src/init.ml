open Types
open Simplify

let rec get_table (modules : module_ array) mi i =
  let tables = modules.(mi).tables in
  match tables.(i) with
  | Local (rt, tbl, max) ->
    (mi, rt, tbl, max, fun tbl -> tables.(i) <- Local (rt, tbl, max))
  | Imported (mi, i) -> get_table modules mi i

let rec get_global (modules : module_ array) mi i =
  let globals = modules.(mi).globals in
  match globals.(i) with
  | Local (gt, g) -> (mi, gt, g, fun g -> globals.(i) <- Local (gt, g))
  | Imported (mi, i) -> get_global modules mi i

let rec get_func (modules : module_ array) mi i =
  let funcs = modules.(mi).funcs in
  match funcs.(i) with
  (* TODO: do we need set somewhere ? *)
  | Local f -> (mi, f, fun f -> funcs.(i) <- Local f)
  | Imported (m, i) -> get_func modules m i

let rec get_memory (modules : module_ array) mi i =
  let memories = modules.(mi).memories in
  match memories.(i) with
  | Local (m, max) -> (mi, m, max, fun m max -> memories.(i) <- Local (m, max))
  | Imported (mi, i) -> get_memory modules mi i

let indice_to_int = function
  | Raw i -> Uint32.to_int i
  | Symbolic id ->
    failwith
    @@ Format.sprintf
         "interpreter internal error (indice_to_int init): unbound id $%s" id

let module_ _registered_modules modules module_indice =
  let m = modules.(module_indice) in
  let const_expr = function
    | [ I32_const n ] -> Const_I32 n
    | [ I64_const n ] -> Const_I64 n
    | [ F32_const f ] -> Const_F32 f
    | [ F64_const f ] -> Const_F64 f
    | [ Ref_null rt ] -> Const_null rt
    | [ Global_get i ] ->
      let _mi, _gt, e, _set =
        get_global modules module_indice (indice_to_int i)
      in
      e
    | [ Ref_func ind ] -> Const_host (indice_to_int ind)
    | e -> failwith @@ Format.asprintf "TODO global expression: `%a`" Pp.expr e
  in

  let const_expr_to_int e =
    match const_expr e with
    | Const_I32 n -> Int32.to_int n
    | _whatever -> failwith "TODO const_expr_to_int"
  in

  ignore
  @@ List.fold_left
       (fun curr_table field ->
         match field with
         | MTable _ -> curr_table + 1
         | MElem e -> (
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
               e.init;
             curr_table
           | _ -> curr_table )
         | _ -> curr_table )
       (-1) m.fields
