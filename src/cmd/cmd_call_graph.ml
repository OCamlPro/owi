(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Stack = Stack.Make (Concrete_value)
module M = Map.Make (Int)
module S = Set.Make (Int)

type mode =
  | Complete
  | Sound

let find_functions_with_func_type func_type (acc, i)
  (f : (Binary.Func.t, Binary.block_type) Origin.t) =
  let (Bt_raw (_, ft)) =
    match f with Origin.Local x -> x.type_f | Origin.Imported imp -> imp.typ
  in
  if Text.func_type_eq func_type ft then (S.add i acc, i + 1) else (acc, i + 1)

let rec find_children mode tables (funcs : 'a array) acc (l : Binary.expr) =
  match (l, mode) with
  | [], _ -> acc
  | ({ raw = Call i | Return_call i; _ } as instr) :: l, _ ->
    Annotated.update_functions_called instr (S.singleton i);
    find_children mode tables funcs (S.add i acc) l
  | ( ( { raw =
            ( Call_indirect (_, Bt_raw (_, ft))
            | Return_call_indirect (_, Bt_raw (_, ft)) )
        ; _
        } as instr )
      :: l
    , Complete ) ->
    let children, _ =
      Array.fold_left (find_functions_with_func_type ft) (S.empty, 0) funcs
    in
    Annotated.update_functions_called instr children;
    let acc = S.union children acc in
    find_children mode tables funcs acc l
  | ( { raw = I32_const x; _ }
      :: ( { raw = Call_indirect (i, _) | Return_call_indirect (i, _); _ } as
           instr )
      :: l
    , Sound ) -> (
    let t_opt = M.find_opt i tables in
    let f =
      Option.bind t_opt (fun t ->
        match Array.get t (Int32.to_int x) with
        | Ok n -> Some (Int32.to_int n)
        | _ -> None )
    in
    match f with
    | Some f ->
      Annotated.update_functions_called instr (S.singleton f);
      find_children mode tables funcs (S.add f acc) l
    | None -> find_children mode tables funcs acc l )
  | { raw = Block (_, _, exp) | Loop (_, _, exp); _ } :: l, _ ->
    let x = find_children mode tables funcs acc exp.raw in
    find_children mode tables funcs x l
  | { raw = If_else (_, _, e1, e2); _ } :: l, _ ->
    let x = find_children mode tables funcs acc e1.raw in
    let x = find_children mode tables funcs x e2.raw in
    find_children mode tables funcs x l
  | _ :: l, _ -> find_children mode tables funcs acc l

let build_graph mode tables funcs (g, i) (f : (Binary.Func.t, 'a) Origin.t) =
  match f with
  | Origin.Local x ->
    let s = find_children mode tables funcs S.empty x.body.raw in
    let cfg = Cmd_cfg.build_cfg_from_func x in
    ((i, Some cfg, s) :: g, i + 1)
  | Origin.Imported _ -> ((i, None, S.empty) :: g, i + 1)

let eval_ibinop stack nn (op : Text.ibinop) =
  match nn with
  | Text.S32 ->
    let (n1, n2), stack = Stack.pop2_i32 stack in
    Stack.push_i32 stack
      (let open Int32 in
       match op with
       | Add -> add n1 n2
       | Sub -> sub n1 n2
       | Mul -> mul n1 n2
       | _ -> assert false )
  | S64 ->
    let (n1, n2), stack = Stack.pop2_i64 stack in
    Stack.push_i64 stack
      (let open Int64 in
       match op with
       | Add -> add n1 n2
       | Sub -> sub n1 n2
       | Mul -> mul n1 n2
       | _ -> assert false )

let get_const_global env id =
  match M.find_opt id env with Some n -> n | None -> assert false

let eval_const_instr env stack instr =
  match instr.Annotated.raw with
  | Binary.I32_const n -> ok @@ Stack.push_i32 stack n
  | I64_const n -> ok @@ Stack.push_i64 stack n
  | F32_const f -> ok @@ Stack.push_f32 stack f
  | F64_const f -> ok @@ Stack.push_f64 stack f
  | V128_const f -> ok @@ Stack.push_v128 stack f
  | I_binop (nn, op) -> ok @@ eval_ibinop stack nn op
  | Ref_null t -> ok @@ Stack.push_ref stack (Concrete_ref.null t)
  | Global_get id ->
    let* g = get_const_global env id in
    ok @@ Stack.push_i32 stack g
  | Ref_func id -> ok @@ Stack.push_i32_of_int stack id
  | _ -> assert false

let eval_const env exp =
  let* stack =
    list_fold_left (eval_const_instr env) Stack.empty exp.Annotated.raw
  in
  match stack with
  | [] -> Error (`Type_mismatch "const expr returning zero values")
  | _ :: _ :: _ ->
    Error (`Type_mismatch "const expr returning more than one value")
  | [ Concrete_value.I32 i ] -> Ok i
  | [ _ ] -> Error (`Type_mismatch "expected int32")

let eval_tables tables env =
  let t =
    List.map
      (fun (n, elem) ->
        (n, Array.of_list (List.map (eval_const env) elem.Binary.Elem.init)) )
      tables
  in
  M.of_list t

let build_env (env, n) (global : (Binary.Global.t, 'a) Origin.t) =
  match global with
  | Origin.Local x -> (
    match fst x.Binary.Global.typ with
    | Const -> (M.add n (eval_const env x.Binary.Global.init) env, n + 1)
    | _ -> (env, n + 1) )
  | _ -> (env, n + 1)

let rec find_tables acc (e : Binary.instr Annotated.t) =
  match e.raw with
  | Table_set i | Table_fill i | Table_copy (i, _) -> i :: acc
  | Block (_, _, exp) | Loop (_, _, exp) ->
    List.fold_left find_tables acc exp.raw
  | If_else (_, _, e1, e2) ->
    let acc = List.fold_left find_tables acc e1.raw in
    List.fold_left find_tables acc e2.raw
  | _ -> acc

let find_tables_to_remove export_tables funcs =
  let l = List.map (fun (x : Binary.Export.t) -> x.id) export_tables in
  Array.fold_left
    (fun acc f ->
      match f with
      | Origin.Local x -> List.fold_left find_tables acc x.Binary.Func.body.raw
      | _ -> acc )
    l funcs

let rec remove_tables (l1 : (int * Binary.Elem.t) list) l2 acc =
  match (l1, l2) with
  | [], [] -> acc
  | [], _ -> acc
  | h :: t, [] -> remove_tables t [] (h :: acc)
  | (n, e) :: t1, h2 :: t2 ->
    if n = h2 then remove_tables t1 t2 acc
    else if n < h2 then remove_tables t1 l2 ((n, e) :: acc)
    else remove_tables l1 t2 acc

let find_entry_points (m : Binary.Module.t) =
  let l = Option.to_list m.start in
  Array.fold_left
    (fun acc (x : Binary.Export.t) -> x.id :: acc)
    l m.exports.func

let find_entries entry_point (m : Binary.Module.t) =
  let entries =
    Option.bind
      (Option.bind entry_point (fun x ->
         Array.find_index
           (function
             | Origin.Local (y : Binary.Func.t) ->
               Option.compare String.compare (Some x) y.id = 0
             | _ -> false )
           m.func ) )
      (fun x -> Some [ x ])
  in
  Option.value entries ~default:(find_entry_points m)

let build_call_graph call_graph_mode (m : Binary.Module.t) entry_point =
  let funcs = m.func in

  let tables =
    match call_graph_mode with
    | Complete -> M.empty
    | Sound ->
      let tables =
        let elems =
          List.filter_map
            (fun e ->
              match e.Binary.Elem.mode with
              | Active (Some n, _) -> Some (n, e)
              | _ -> None )
            (Array.to_list m.elem)
        in
        let t = find_tables_to_remove (Array.to_list m.exports.table) funcs in
        remove_tables
          (List.sort_uniq (fun x y -> compare (fst x) (fst y)) elems)
          (List.sort_uniq compare t) []
      in
      let env, _ = Array.fold_left build_env (M.empty, 0) m.global in

      eval_tables tables env
  in

  let l, _ =
    Array.fold_left (build_graph call_graph_mode tables funcs) ([], 0) funcs
  in
  let entries = find_entries entry_point m in
  Call_graph.init l entries

let build_call_graph_from_text_module call_graph_mode modul entry_point =
  let m = Compile.Text.until_validate ~unsafe:false modul in
  match m with
  | Ok m -> build_call_graph call_graph_mode m entry_point
  | _ -> assert false

let compute_distances m entry_point =
  let call_graph = build_call_graph Sound m entry_point in
  Exploration_smart.compute_distance_to_unreachable call_graph

let cmd ~call_graph_mode ~source_file ~entry_point =
  let* m = Compile.File.until_validate ~unsafe:false source_file in
  let call_graph = build_call_graph call_graph_mode m entry_point in

  Bos.OS.File.writef
    (Fpath.set_ext ".dot" source_file)
    "%a" Call_graph.pp call_graph
