(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let rewrite_block_type (assigned : Assigned.t) (block_type : Text.block_type) :
  Binary.block_type Result.t =
  match block_type with
  | Bt_ind id ->
    let* idx = Assigned.find_type assigned id in
    let+ t =
      match Assigned.get_type assigned idx with
      | None -> Error (`Unknown_type id)
      | Some v -> Ok v
    in
    Binary.Bt_raw (Some idx, t)
  | Bt_raw (_, func_type) ->
    let idx = Assigned.find_raw_type assigned func_type in
    Ok (Binary.Bt_raw (Some idx, func_type))

let rewrite_expr (assigned : Assigned.t) (locals : Text.param list)
  (iexpr : Text.expr Annotated.t) : Binary.expr Annotated.t Result.t =
  (* block_ids handling *)
  let block_id_to_raw (loop_count, block_ids) id =
    let* id =
      match id with
      | Text.Text id -> begin
        match
          List.find_index
            (function Some id' -> String.equal id id' | None -> false)
            block_ids
        with
        | None -> Error (`Unknown_label (Text.Text id))
        | Some id -> Ok id
      end
      | Raw id -> Ok id
    in
    (* this is > and not >= because you can `br 0` without any block to target the function *)
    if id > List.length block_ids + loop_count then
      Error (`Unknown_label (Text.Raw id))
    else Ok id
  in

  (* block_types handling *)
  let block_ty_opt_rewrite = function
    | Some bt ->
      let+ bt = rewrite_block_type assigned bt in
      Some bt
    | None -> Ok None
  in

  let seen_locals = Hashtbl.create 64 in

  (* Fill locals *)
  let* (_ : int) =
    list_fold_left
      (fun next_free_int ((name, _type) : Text.param) ->
        match name with
        | None -> Ok (next_free_int + 1)
        | Some name ->
          if Hashtbl.mem seen_locals name then Error (`Duplicate_local name)
          else begin
            Hashtbl.add seen_locals name next_free_int;
            Ok (next_free_int + 1)
          end )
      0 locals
  in

  let find_local : Text.indice -> Binary.indice = function
    | Raw i -> i
    | Text name -> (
      match Hashtbl.find_opt seen_locals name with
      | None -> assert false
      | Some id -> id )
  in

  let rec body (loop_count, block_ids) (instr : Text.instr Annotated.t) :
    Binary.instr Result.t =
    match instr.Annotated.raw with
    | Br_table (ids, id) ->
      let block_id_to_raw = block_id_to_raw (loop_count, block_ids) in
      let* ids = array_map block_id_to_raw ids in
      let+ id = block_id_to_raw id in
      Binary.Br_table (ids, id)
    | Br_if id ->
      let+ id = block_id_to_raw (loop_count, block_ids) id in
      Binary.Br_if id
    | Br id ->
      let+ id = block_id_to_raw (loop_count, block_ids) id in
      Binary.Br id
    | Call id ->
      let+ id = Assigned.find_func assigned id in
      Binary.Call id
    | Return_call id ->
      let+ id = Assigned.find_func assigned id in
      Binary.Return_call id
    | Local_set id ->
      let id = find_local id in
      Ok (Binary.Local_set id)
    | Local_get id ->
      let id = find_local id in
      Ok (Binary.Local_get id)
    | Local_tee id ->
      let id = find_local id in
      Ok (Binary.Local_tee id)
    | If_else (id, bt, e1, e2) ->
      let* bt = block_ty_opt_rewrite bt in
      let block_ids = id :: block_ids in
      let* e1 = expr e1 (loop_count, block_ids) in
      let+ e2 = expr e2 (loop_count, block_ids) in
      Binary.If_else (id, bt, e1, e2)
    | Loop (id, bt, e) ->
      let* bt = block_ty_opt_rewrite bt in
      let+ e = expr e (loop_count + 1, id :: block_ids) in
      Binary.Loop (id, bt, e)
    | Block (id, bt, e) ->
      let* bt = block_ty_opt_rewrite bt in
      let+ e = expr e (loop_count, id :: block_ids) in
      Binary.Block (id, bt, e)
    | Call_indirect (tbl_i, bt) ->
      let* tbl_i = Assigned.find_table assigned tbl_i in
      let+ bt = rewrite_block_type assigned bt in
      Binary.Call_indirect (tbl_i, bt)
    | Return_call_indirect (tbl_i, bt) ->
      let* tbl_i = Assigned.find_table assigned tbl_i in
      let+ bt = rewrite_block_type assigned bt in
      Binary.Return_call_indirect (tbl_i, bt)
    | Call_ref t ->
      let+ t = Assigned.find_type assigned t in
      Binary.Call_ref t
    | Return_call_ref bt ->
      let+ bt = rewrite_block_type assigned bt in
      Binary.Return_call_ref bt
    | Global_set id ->
      let+ idx = Assigned.find_global assigned id in
      Binary.Global_set idx
    | Global_get id ->
      let+ idx = Assigned.find_global assigned id in
      Binary.Global_get idx
    | Ref_func id ->
      let+ id = Assigned.find_func assigned id in
      Binary.Ref_func id
    | Table_size id ->
      let+ id = Assigned.find_table assigned id in
      Binary.Table_size id
    | Table_get id ->
      let+ id = Assigned.find_table assigned id in
      Binary.Table_get id
    | Table_set id ->
      let+ id = Assigned.find_table assigned id in
      Binary.Table_set id
    | Table_grow id ->
      let+ id = Assigned.find_table assigned id in
      Binary.Table_grow id
    | Table_init (i, i') ->
      let* table = Assigned.find_table assigned i in
      let+ elem = Assigned.find_elem assigned i' in
      Binary.Table_init (table, elem)
    | Table_fill id ->
      let+ id = Assigned.find_table assigned id in
      Binary.Table_fill id
    | Table_copy (i, i') ->
      let* table = Assigned.find_table assigned i in
      let+ table' = Assigned.find_table assigned i' in
      Binary.Table_copy (table, table')
    | Memory_init (memidx, dataidx) ->
      let* memidx = Assigned.find_memory assigned memidx in
      let+ dataidx = Assigned.find_data assigned dataidx in
      Binary.Memory_init (memidx, dataidx)
    | Data_drop id ->
      let+ id = Assigned.find_data assigned id in
      Binary.Data_drop id
    | Elem_drop id ->
      let+ id = Assigned.find_elem assigned id in
      Binary.Elem_drop id
    | Select typ -> begin
      match typ with
      | None -> Ok (Binary.Select None)
      | Some [ t ] -> Ok (Binary.Select (Some [ t ]))
      | Some [] | Some (_ :: _ :: _) -> Error `Invalid_result_arity
    end
    | I_unop (nn, op) -> Ok (Binary.I_unop (nn, op))
    | I_binop (nn, op) -> Ok (I_binop (nn, op))
    | I_testop (nn, op) -> Ok (Binary.I_testop (nn, op))
    | I_relop (nn, op) -> Ok (I_relop (nn, op))
    | F_unop (nn, op) -> Ok (Binary.F_unop (nn, op))
    | F_relop (nn, op) -> Ok (F_relop (nn, op))
    | I32_wrap_i64 -> Ok Binary.I32_wrap_i64
    | F_reinterpret_i (nn1, nn2) -> Ok (F_reinterpret_i (nn1, nn2))
    | I_reinterpret_f (nn1, nn2) -> Ok (I_reinterpret_f (nn1, nn2))
    | I64_extend_i32 sx -> Ok (Binary.I64_extend_i32 sx)
    | I64_extend32_s -> Ok Binary.I64_extend32_s
    | F32_demote_f64 -> Ok Binary.F32_demote_f64
    | I_extend8_s nn -> Ok (I_extend8_s nn)
    | I_extend16_s nn -> Ok (I_extend16_s nn)
    | F64_promote_f32 -> Ok Binary.F64_promote_f32
    | F_convert_i (nn1, nn2, sx) -> Ok (Binary.F_convert_i (nn1, nn2, sx))
    | I_trunc_f (nn1, nn2, sx) -> Ok (Binary.I_trunc_f (nn1, nn2, sx))
    | I_trunc_sat_f (nn1, nn2, sx) -> Ok (Binary.I_trunc_sat_f (nn1, nn2, sx))
    | Ref_is_null -> Ok Binary.Ref_is_null
    | F_binop (nn, op) -> Ok (Binary.F_binop (nn, op))
    | F32_const v -> Ok (Binary.F32_const v)
    | F64_const v -> Ok (Binary.F64_const v)
    | I32_const v -> Ok (Binary.I32_const v)
    | I64_const v -> Ok (Binary.I64_const v)
    | V128_const v -> Ok (Binary.V128_const v)
    | Unreachable -> Ok Binary.Unreachable
    | Drop -> Ok Binary.Drop
    | Nop -> Ok Binary.Nop
    | Return -> Ok Binary.Return
    | Extern_externalize -> Ok Binary.Extern_externalize
    | Extern_internalize -> Ok Binary.Extern_internalize
    | I_load8 (id, nn, sx, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I_load8 (id, nn, sx, memarg)
    | I_store8 (id, nn, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I_store8 (id, nn, memarg)
    | I_load16 (id, nn, sx, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I_load16 (id, nn, sx, memarg)
    | I_store16 (id, nn, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I_store16 (id, nn, memarg)
    | I64_load32 (id, sx, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I64_load32 (id, sx, memarg)
    | I64_store32 (id, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I64_store32 (id, memarg)
    | I_load (id, nn, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I_load (id, nn, memarg)
    | F_load (id, nn, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.F_load (id, nn, memarg)
    | F_store (id, nn, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.F_store (id, nn, memarg)
    | I_store (id, nn, memarg) ->
      let+ id = Assigned.find_memory assigned id in
      Binary.I_store (id, nn, memarg)
    | Memory_copy (id1, id2) ->
      let* id1 = Assigned.find_memory assigned id1 in
      let+ id2 = Assigned.find_memory assigned id2 in
      Binary.Memory_copy (id1, id2)
    | Memory_size id ->
      let+ id = Assigned.find_memory assigned id in
      Binary.Memory_size id
    | Memory_fill id ->
      let+ id = Assigned.find_memory assigned id in
      Binary.Memory_fill id
    | Memory_grow id ->
      let+ id = Assigned.find_memory assigned id in
      Binary.Memory_grow id
    | V_ibinop (shape, op) -> Ok (Binary.V_ibinop (shape, op))
    | Ref_null t -> Ok (Binary.Ref_null t)
  and expr (e : Text.expr Annotated.t) (loop_count, block_ids) :
    Binary.expr Annotated.t Result.t =
    let+ e =
      list_map
        (fun i ->
          let+ i = body (loop_count, block_ids) i in
          Annotated.dummy i )
        e.Annotated.raw
    in
    Annotated.dummy e
  in
  expr iexpr (0, [])

let rewrite_global (assigned : Assigned.t) (global : Text.Global.t) :
  Binary.Global.t Result.t =
  let+ init = rewrite_expr assigned [] global.init in
  { Binary.Global.id = global.id; init; typ = global.typ }

let rewrite_elem (assigned : Assigned.t) (elem : Text.Elem.t) :
  Binary.Elem.t Result.t =
  let* (mode : Binary.Elem.Mode.t) =
    match elem.mode with
    | Declarative -> Ok Binary.Elem.Mode.Declarative
    | Passive -> Ok Passive
    | Active (None, _expr) -> assert false
    | Active (Some id, expr) ->
      let* indice = Assigned.find_table assigned id in
      let+ expr = rewrite_expr assigned [] expr in
      Binary.Elem.Mode.Active (Some indice, expr)
  in
  let+ init = list_map (rewrite_expr assigned []) elem.init in
  { Binary.Elem.init; mode; id = elem.id; typ = elem.typ }

let rewrite_data (assigned : Assigned.t) (data : Text.Data.t) :
  Binary.Data.t Result.t =
  let+ mode =
    match data.mode with
    | Passive -> Ok Binary.Data.Mode.Passive
    | Active (None, _expr) -> assert false
    | Active (Some indice, expr) ->
      let* indice = Assigned.find_memory assigned indice in
      let+ expr = rewrite_expr assigned [] expr in
      Binary.Data.Mode.Active (indice, expr)
  in
  { Binary.Data.mode; id = data.id; init = data.init }

let rewrite_export find assigned (exports : Grouped.opt_export Array.t) :
  Binary.Export.t Array.t Result.t =
  array_map
    (fun { Grouped.name; id } ->
      match find assigned id with
      | Error _ -> Error (`Unknown_export id)
      | Ok id -> Ok { Binary.Export.name; id } )
    exports

let rewrite_exports (modul : Grouped.t) (assigned : Assigned.t) :
  Binary.Module.Exports.t Result.t =
  let* global =
    rewrite_export Assigned.find_global assigned modul.global_exports
  in
  let* mem = rewrite_export Assigned.find_memory assigned modul.mem_exports in
  let* table =
    rewrite_export Assigned.find_table assigned modul.table_exports
  in
  let+ func = rewrite_export Assigned.find_func assigned modul.func_exports in
  { Binary.Module.Exports.global; mem; table; func }

let rewrite_func (assigned : Assigned.t)
  ({ id; type_f; locals; body; _ } : Text.Func.t) : Binary.Func.t Result.t =
  let* (Bt_raw (_, (params, _)) as type_f) =
    rewrite_block_type assigned type_f
  in
  let+ body = rewrite_expr assigned (params @ locals) body in
  { Binary.Func.body; type_f; id; locals }

let rewrite_types (t : Text.func_type) : Text.Typedef.t Result.t = Ok (None, t)

let modul (modul : Grouped.t) (assigned : Assigned.t) : Binary.Module.t Result.t
    =
  Log.debug (fun m -> m "rewriting    ...");
  let* global =
    array_map
      (Origin.monadic_map ~f_local:(rewrite_global assigned)
         ~f_imported:Result.ok )
      modul.global
  in
  let* elem = array_map (rewrite_elem assigned) modul.elem in
  let* data = array_map (rewrite_data assigned) modul.data in
  let* exports = rewrite_exports modul assigned in
  let* func =
    let f_imported = rewrite_block_type assigned in
    let f_local = rewrite_func assigned in
    let runtime = Origin.monadic_map ~f_local ~f_imported in
    array_map runtime modul.func
  in
  let* types = array_map rewrite_types (Assigned.get_types assigned) in
  let+ start =
    match modul.start with
    | None -> Ok None
    | Some id ->
      let+ id = Assigned.find_func assigned id in
      Some id
  in

  { Binary.Module.id = modul.id
  ; mem = modul.mem
  ; table = modul.table
  ; types
  ; global
  ; elem
  ; data
  ; exports
  ; func
  ; start
  ; custom = []
  }
