(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax

module Type = struct
  type t = func_type

  let compare (x : t) (y : t) = Types.compare_func_type x y
end

module TypeMap = Map.Make (Type)

let typemap (types : Type.t Named.t) =
  Named.fold
    (fun idx typ acc -> TypeMap.add typ (Raw idx) acc)
    types TypeMap.empty

let rewrite_block_type (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (block_type : text block_type) : binary block_type Result.t =
  match block_type with
  | Bt_ind id -> begin
    let* (Raw v) = Assigned.find_type modul id in
    match List.nth_opt modul.typ.values v with
    | None -> Error (`Unknown_type id)
    | Some v ->
      let t' = Indexed.get v in
      let idx = Indexed.get_index v in
      Ok (Bt_raw (Some (Raw idx), t'))
  end
  | Bt_raw (_, func_type) ->
    let idx =
      match TypeMap.find_opt func_type typemap with
      | None -> assert false
      | Some idx -> idx
    in
    Ok (Bt_raw (Some idx, func_type))

let rewrite_expr (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (locals : param list) (iexpr : text expr Annotated.t) :
  binary expr Annotated.t Result.t =
  (* block_ids handling *)
  let block_id_to_raw (loop_count, block_ids) id =
    let* id =
      match id with
      | Text id -> begin
        match
          List.find_index
            (function Some id' -> String.equal id id' | None -> false)
            block_ids
        with
        | None -> Error (`Unknown_label (Text id))
        | Some id -> Ok id
      end
      | Raw id -> Ok id
    in
    (* this is > and not >= because you can `br 0` without any block to target the function *)
    if id > List.length block_ids + loop_count then
      Error (`Unknown_label (Raw id))
    else Ok (Raw id)
  in

  (* block_types handling *)
  let block_ty_opt_rewrite = function
    | Some bt ->
      let+ bt = rewrite_block_type typemap modul bt in
      Some bt
    | None -> Ok None
  in

  let* locals, _after_last_assigned_local =
    list_fold_left
      (fun (locals, next_free_int) ((name, _type) : param) ->
        match name with
        | None -> Ok (locals, next_free_int + 1)
        | Some name ->
          if String_map.mem name locals then Error (`Duplicate_local name)
          else Ok (String_map.add name next_free_int locals, next_free_int + 1) )
      (String_map.empty, 0) locals
  in

  let find_local = function
    | Raw _i as id -> id
    | Text name -> (
      match String_map.find_opt name locals with
      | None -> assert false
      | Some id -> Raw id )
  in

  let rec body (loop_count, block_ids) (instr : text instr Annotated.t) :
    binary instr Result.t =
    match instr.Annotated.raw with
    | Br_table (ids, id) ->
      let block_id_to_raw = block_id_to_raw (loop_count, block_ids) in
      let* ids = array_map block_id_to_raw ids in
      let+ id = block_id_to_raw id in
      Br_table (ids, id)
    | Br_if id ->
      let+ id = block_id_to_raw (loop_count, block_ids) id in
      Br_if id
    | Br id ->
      let+ id = block_id_to_raw (loop_count, block_ids) id in
      Br id
    | Call id ->
      let+ id = Assigned.find_func modul id in
      Call id
    | Return_call id ->
      let+ id = Assigned.find_func modul id in
      Return_call id
    | Local_set id ->
      let id = find_local id in
      Ok (Local_set id)
    | Local_get id ->
      let id = find_local id in
      Ok (Local_get id)
    | Local_tee id ->
      let id = find_local id in
      Ok (Local_tee id)
    | If_else (id, bt, e1, e2) ->
      let* bt = block_ty_opt_rewrite bt in
      let block_ids = id :: block_ids in
      let* e1 = expr e1 (loop_count, block_ids) in
      let+ e2 = expr e2 (loop_count, block_ids) in
      If_else (id, bt, e1, e2)
    | Loop (id, bt, e) ->
      let* bt = block_ty_opt_rewrite bt in
      let+ e = expr e (loop_count + 1, id :: block_ids) in
      Loop (id, bt, e)
    | Block (id, bt, e) ->
      let* bt = block_ty_opt_rewrite bt in
      let+ e = expr e (loop_count, id :: block_ids) in
      Block (id, bt, e)
    | Call_indirect (tbl_i, bt) ->
      let* tbl_i = Assigned.find_table modul tbl_i in
      let+ bt = rewrite_block_type typemap modul bt in
      Call_indirect (tbl_i, bt)
    | Return_call_indirect (tbl_i, bt) ->
      let* tbl_i = Assigned.find_table modul tbl_i in
      let+ bt = rewrite_block_type typemap modul bt in
      Return_call_indirect (tbl_i, bt)
    | Call_ref t ->
      let+ t = Assigned.find_type modul t in
      Call_ref t
    | Return_call_ref bt ->
      let+ bt = rewrite_block_type typemap modul bt in
      Return_call_ref bt
    | Global_set id ->
      let+ idx = Assigned.find_global modul id in
      Global_set idx
    | Global_get id ->
      let+ idx = Assigned.find_global modul id in
      Global_get idx
    | Ref_func id ->
      let+ id = Assigned.find_func modul id in
      Ref_func id
    | Table_size id ->
      let+ id = Assigned.find_table modul id in
      Table_size id
    | Table_get id ->
      let+ id = Assigned.find_table modul id in
      Table_get id
    | Table_set id ->
      let+ id = Assigned.find_table modul id in
      Table_set id
    | Table_grow id ->
      let+ id = Assigned.find_table modul id in
      Table_grow id
    | Table_init (i, i') ->
      let* table = Assigned.find_table modul i in
      let+ elem = Assigned.find_elem modul i' in
      Table_init (table, elem)
    | Table_fill id ->
      let+ id = Assigned.find_table modul id in
      Table_fill id
    | Table_copy (i, i') ->
      let* table = Assigned.find_table modul i in
      let+ table' = Assigned.find_table modul i' in
      Table_copy (table, table')
    | Memory_init id ->
      let+ id = Assigned.find_data modul id in
      Memory_init id
    | Data_drop id ->
      let+ id = Assigned.find_data modul id in
      Data_drop id
    | Elem_drop id ->
      let+ id = Assigned.find_elem modul id in
      Elem_drop id
    | Select typ -> begin
      match typ with
      | None -> Ok (Select None)
      | Some [ t ] -> Ok (Select (Some [ t ]))
      | Some [] | Some (_ :: _ :: _) -> Error `Invalid_result_arity
    end
    | ( I_unop _ | I_binop _ | I_testop _ | I_relop _ | F_unop _ | F_relop _
      | I32_wrap_i64 | F_reinterpret_i _ | I_reinterpret_f _ | I64_extend_i32 _
      | I64_extend32_s | F32_demote_f64 | I_extend8_s _ | I_extend16_s _
      | F64_promote_f32 | F_convert_i _ | I_trunc_f _ | I_trunc_sat_f _
      | Ref_is_null | F_binop _ | F32_const _ | F64_const _ | I32_const _
      | I64_const _ | V128_const _ | Unreachable | Drop | Nop | Return
      | Extern_externalize | Extern_internalize | I_load8 _ | I_store8 _
      | I_load16 _ | I_store16 _ | I64_load32 _ | I64_store32 _ | I_load _
      | F_load _ | F_store _ | I_store _ | Memory_copy | Memory_size
      | Memory_fill | Memory_grow | V_ibinop _ ) as i ->
      Ok i
    | Ref_null t -> Ok (Ref_null t)
  and expr (e : text expr Annotated.t) (loop_count, block_ids) :
    binary expr Annotated.t Result.t =
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

let rewrite_global (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (global : Text.global) : Binary.global Result.t =
  let+ init = rewrite_expr typemap modul [] global.init in
  let mut, val_type = global.typ in
  let typ = (mut, val_type) in
  { Binary.id = global.id; init; typ }

let rewrite_elem (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (elem : Text.elem) : Binary.elem Result.t =
  let* (mode : Binary.elem_mode) =
    match elem.mode with
    | Elem_declarative -> Ok Binary.Elem_declarative
    | Elem_passive -> Ok Elem_passive
    | Elem_active (None, _expr) -> assert false
    | Elem_active (Some id, expr) ->
      let* (Raw indice) = Assigned.find_table modul id in
      let+ expr = rewrite_expr typemap modul [] expr in
      Binary.Elem_active (Some indice, expr)
  in
  let+ init = list_map (rewrite_expr typemap modul []) elem.init in
  { Binary.init; mode; id = elem.id; typ = elem.typ }

let rewrite_data (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (data : Text.data) : Binary.data Result.t =
  let+ mode =
    match data.mode with
    | Data_passive -> Ok Binary.Data_passive
    | Data_active (None, _expr) -> assert false
    | Data_active (Some indice, expr) ->
      let* (Raw indice) = Assigned.find_memory modul indice in
      let+ expr = rewrite_expr typemap modul [] expr in
      Binary.Data_active (indice, expr)
  in
  { Binary.mode; id = data.id; init = data.init }

let rewrite_export named (exports : Grouped.opt_export list) :
  Binary.export list Result.t =
  list_map
    (fun { Grouped.name; id } ->
      let+ id =
        match id with
        | Raw _i as indice -> Ok indice
        | Text name -> (
          match String_map.find_opt name named.Named.named with
          | None -> Error (`Unknown_export id)
          | Some i -> Ok (Raw i) )
      in
      let (Raw id) : binary indice = id in

      { Binary.name; id } )
    exports

let rewrite_exports (modul : Assigned.t) (exports : Grouped.opt_exports) :
  Binary.exports Result.t =
  let* global = rewrite_export modul.global exports.global in
  let* mem = rewrite_export modul.mem exports.mem in
  let* table = rewrite_export modul.table exports.table in
  let+ func = rewrite_export modul.func exports.func in
  { Binary.global; mem; table; func }

let rewrite_func (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  ({ id; type_f; locals; body; _ } : text func) : binary func Result.t =
  let* (Bt_raw (_, (params, _)) as type_f) =
    rewrite_block_type typemap modul type_f
  in
  let+ body = rewrite_expr typemap modul (params @ locals) body in
  { body; type_f; id; locals }

let rewrite_import (f : 'a -> 'b Result.t) (import : 'a Imported.t) :
  'b Imported.t Result.t =
  let+ desc = f import.desc in
  { import with desc }

let rewrite_runtime f g r =
  match r with
  | Runtime.Local v ->
    let+ v = f v in
    Runtime.Local v
  | Imported i ->
    let+ i = g i in
    Runtime.Imported i

let rewrite_named f named =
  let+ values =
    list_map
      (fun ind ->
        let index = Indexed.get_index ind in
        let value = Indexed.get ind in
        let+ value = f value in
        Indexed.return index value )
      named.Named.values
  in
  Named.create values named.named

let rewrite_types (_modul : Assigned.t) (t : func_type) : type_def Result.t =
  Ok (None, t)

let rec rewrite_term ~(binder_list : string option list) ~(modul : Assigned.t)
  ~(func_param_list : string option list) :
  text Spec.term -> binary Spec.term Result.t =
  let rec find_raw_indice error acc id = function
    | [] -> Error error
    | Some id' :: bl ->
      if String.equal id id' then Ok (Raw acc)
      else find_raw_indice error (acc + 1) id bl
    | None :: bl -> find_raw_indice error (acc + 1) id bl
  in

  let find_binder (binder_list : string option list) (ind : text indice) :
    binary indice Result.t =
    match ind with
    | Raw id -> Ok (Raw id)
    | Text id -> find_raw_indice (`Spec_unknown_binder ind) 0 id binder_list
  in

  let find_param (func_param_list : string option list) (ind : text indice) :
    binary indice Result.t =
    match ind with
    | Raw id -> Ok (Raw id)
    | Text id -> find_raw_indice (`Spec_unknown_param ind) 0 id func_param_list
  in

  let open Spec in
  function
  | Int32 i32 -> Ok (Int32 i32)
  | Int64 i64 -> Ok (Int64 i64)
  | Float32 f32 -> Ok (Float32 f32)
  | Float64 f64 -> Ok (Float64 f64)
  | Var ind -> (
    match
      ( find_binder binder_list ind
      , find_param func_param_list ind
      , Assigned.find_global modul ind )
    with
    | Ok ind, _, _ -> Ok (BinderVar ind)
    | _, Ok ind, _ -> Ok (ParamVar ind)
    | _, _, Ok ind -> Ok (GlobalVar ind)
    | _, _, _ -> Error (`Spec_unknown_variable ind) )
  | ParamVar ind ->
    let+ ind = find_param func_param_list ind in
    ParamVar ind
  | GlobalVar ind ->
    let+ ind = Assigned.find_global modul ind in
    GlobalVar ind
  | BinderVar ind ->
    let+ ind = find_binder binder_list ind in
    BinderVar ind
  | UnOp (u, tm1) ->
    let+ tm1 = rewrite_term ~binder_list ~modul ~func_param_list tm1 in
    UnOp (u, tm1)
  | BinOp (b, tm1, tm2) ->
    let* tm1 = rewrite_term ~binder_list ~modul ~func_param_list tm1 in
    let+ tm2 = rewrite_term ~binder_list ~modul ~func_param_list tm2 in
    BinOp (b, tm1, tm2)
  | Result i -> Ok (Result i)
  | Memory tm1 ->
    let+ tm1 = rewrite_term ~binder_list ~modul ~func_param_list tm1 in
    Memory tm1

let rec rewrite_prop ~(binder_list : string option list) ~(modul : Assigned.t)
  ~(func_param_list : string option list) :
  text Spec.prop -> binary Spec.prop Result.t =
  let open Spec in
  function
  | Const b -> Ok (Const b)
  | BinPred (b, tm1, tm2) ->
    let* tm1 = rewrite_term ~binder_list ~modul ~func_param_list tm1 in
    let+ tm2 = rewrite_term ~binder_list ~modul ~func_param_list tm2 in
    BinPred (b, tm1, tm2)
  | UnConnect (u, pr1) ->
    let+ pr1 = rewrite_prop ~binder_list ~modul ~func_param_list pr1 in
    UnConnect (u, pr1)
  | BinConnect (b, pr1, pr2) ->
    let* pr1 = rewrite_prop ~binder_list ~modul ~func_param_list pr1 in
    let+ pr2 = rewrite_prop ~binder_list ~modul ~func_param_list pr2 in
    BinConnect (b, pr1, pr2)
  | Binder (b, bt, id_opt, pr1) ->
    let binder_list = id_opt :: binder_list in
    let+ pr1 = rewrite_prop ~binder_list ~modul ~func_param_list pr1 in
    Binder (b, bt, id_opt, pr1)

let rewrite_contract (modul : Assigned.t) :
  text Contract.t -> binary Contract.t Result.t =
 fun { Contract.funcid; preconditions; postconditions } ->
  let* funcid = Assigned.find_func modul funcid in
  let* func =
    let (Raw i) = funcid in
    match Indexed.get_at i modul.func.values with
    | Some v -> Ok v
    | None -> Error (`Spec_invalid_indice (Int.to_string i))
  in
  let func_bt =
    match func with
    | Local { type_f; _ } -> type_f
    | Imported { desc; _ } -> desc
  in
  let* func_param_list =
    match func_bt with
    | Bt_ind ind -> (
      let* (Raw i) = Assigned.find_type modul ind in
      match Indexed.get_at i modul.typ.values with
      | Some (func_pt, _) -> Ok (List.map fst func_pt)
      | _ -> Error (`Spec_invalid_indice (Int.to_string i)) )
    | Bt_raw (_, (func_pt, _)) -> Ok (List.map fst func_pt)
  in
  let* preconditions =
    list_map
      (rewrite_prop ~binder_list:[] ~modul ~func_param_list)
      preconditions
  in
  let+ postconditions =
    list_map
      (rewrite_prop ~binder_list:[] ~modul ~func_param_list)
      postconditions
  in
  { Contract.funcid; preconditions; postconditions }

let rewrite_annot (modul : Assigned.t) :
  text Annot.annot -> Binary.custom Result.t = function
  | Contract contract ->
    let+ contract = rewrite_contract modul contract in
    Binary.From_annot (Contract contract)
  | Annot annot -> ok @@ Binary.From_annot (Annot annot)

let rewrite_annots (modul : Assigned.t) :
  text Annot.annot list -> Binary.custom list Result.t =
  list_map (rewrite_annot modul)

let modul (modul : Assigned.t) : Binary.Module.t Result.t =
  Log.debug (fun m -> m "rewriting    ...");
  let typemap = typemap modul.typ in
  let* global =
    let+ { Named.named; values } =
      rewrite_named
        (rewrite_runtime (rewrite_global typemap modul) ok)
        modul.global
    in
    let values = List.rev values in
    Named.create values named
  in
  let* elem = rewrite_named (rewrite_elem typemap modul) modul.elem in
  let* data = rewrite_named (rewrite_data typemap modul) modul.data in
  let* exports = rewrite_exports modul modul.exports in
  let* func =
    let import = rewrite_import (rewrite_block_type typemap modul) in
    let runtime = rewrite_runtime (rewrite_func typemap modul) import in
    rewrite_named runtime modul.func
  in
  let* types = rewrite_named (rewrite_types modul) modul.typ in
  let* start =
    match modul.start with
    | None -> Ok None
    | Some id ->
      let+ (Raw id) = Assigned.find_func modul id in
      Some id
  in
  let+ custom = rewrite_annots modul modul.annots in

  let id = modul.id in
  let mem = Named.to_array modul.mem in
  let table = Named.to_array modul.table in
  let types = Named.to_array types in
  let global = Named.to_array global in
  let elem = Named.to_array elem in
  let data = Named.to_array data in
  let func = Named.to_array func in

  { Binary.Module.id
  ; mem
  ; table
  ; types
  ; global
  ; elem
  ; data
  ; exports
  ; func
  ; start
  ; custom
  }
