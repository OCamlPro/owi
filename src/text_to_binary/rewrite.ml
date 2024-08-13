(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax

module StrType = struct
  type t = binary str_type

  let compare (x : t) (y : t) = Types.compare_str_type x y
end

module TypeMap = Map.Make (StrType)

let typemap (types : binary str_type Named.t) =
  Named.fold
    (fun idx typ acc -> TypeMap.add typ (Raw idx) acc)
    types TypeMap.empty

let find (named : 'a Named.t) : _ -> binary indice = function
  | Raw _i as indice -> indice
  | Text name -> (
    match String_map.find_opt name named.named with
    | None -> assert false
    | Some i -> Raw i )

let get error (named : 'a Named.t) indice : 'a Indexed.t Result.t =
  let (Raw i) = find named indice in
  (* TODO change Named.t structure to make that sensible *)
  match List.nth_opt named.values i with None -> Error error | Some v -> Ok v

let find_global (modul : Assigned.t) id : binary indice = find modul.global id

let find_memory (modul : Assigned.t) id : binary indice = find modul.mem id

let rewrite_block_type (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (block_type : text block_type) : binary block_type Result.t =
  match block_type with
  | Bt_ind id -> begin
    let+ v = get (`Unknown_type id) modul.typ id in
    match Indexed.get v with
    | Def_func_t t' ->
      let idx = Indexed.get_index v in
      Bt_raw (Some (Raw idx), t')
    | _ -> assert false
  end
  | Bt_raw (_, func_type) ->
    let+ t = Binary_types.convert_func_type None func_type in
    let idx =
      match TypeMap.find_opt (Def_func_t t) typemap with
      | None -> assert false
      | Some idx -> idx
    in
    Bt_raw (Some idx, t)

let rewrite_expr (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (locals : binary param list) (iexpr : text expr) : binary expr Result.t =
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
      (fun (locals, next_free_int) ((name, _type) : binary param) ->
        match name with
        | None -> Ok (locals, next_free_int + 1)
        | Some name ->
          if String_map.mem name locals then Error (`Duplicate_local name)
          else Ok (String_map.add name next_free_int locals, next_free_int + 1)
        )
      (String_map.empty, 0) locals
  in

  let find_local = function
    | Raw _i as id -> id
    | Text name -> (
      match String_map.find_opt name locals with
      | None -> assert false
      | Some id -> Raw id )
  in

  let find_table id = find modul.table id in
  let find_func id = find modul.func id in
  let find_data id = find modul.data id in
  let find_elem id = find modul.elem id in
  let find_type id = find modul.typ id in

  let rec body (loop_count, block_ids) : text instr -> binary instr Result.t =
    function
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
      let id = find_func id in
      Ok (Call id)
    | Return_call id ->
      let id = find_func id in
      Ok (Return_call id)
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
      let tbl_i = find_table tbl_i in
      let+ bt = rewrite_block_type typemap modul bt in
      Call_indirect (tbl_i, bt)
    | Return_call_indirect (tbl_i, bt) ->
      let tbl_i = find_table tbl_i in
      let+ bt = rewrite_block_type typemap modul bt in
      Return_call_indirect (tbl_i, bt)
    | Call_ref t ->
      let t = find_type t in
      Ok (Call_ref t)
    | Return_call_ref bt ->
      let+ bt = rewrite_block_type typemap modul bt in
      Return_call_ref bt
    | Global_set id ->
      let idx = find_global modul id in
      Ok (Global_set idx)
    | Global_get id ->
      let idx = find_global modul id in
      Ok (Global_get idx)
    | Ref_func id ->
      let id = find_func id in
      Ok (Ref_func id)
    | Table_size id ->
      let id = find_table id in
      Ok (Table_size id)
    | Table_get id ->
      let id = find_table id in
      Ok (Table_get id)
    | Table_set id ->
      let id = find_table id in
      Ok (Table_set id)
    | Table_grow id ->
      let id = find_table id in
      Ok (Table_grow id)
    | Table_init (i, i') ->
      let table = find_table i in
      let elem = find_elem i' in
      Ok (Table_init (table, elem))
    | Table_fill id ->
      let id = find_table id in
      Ok (Table_fill id)
    | Table_copy (i, i') ->
      let table = find_table i in
      let table' = find_table i' in
      Ok (Table_copy (table, table'))
    | Memory_init id ->
      let id = find_data id in
      Ok (Memory_init id)
    | Data_drop id ->
      let id = find_data id in
      Ok (Data_drop id)
    | Elem_drop id ->
      let id = find_elem id in
      Ok (Elem_drop id)
    | Select typ -> begin
      match typ with
      | None -> Ok (Select None)
      | Some [ t ] ->
        let+ t = Binary_types.convert_val_type None t in
        Select (Some [ t ])
      | Some [] | Some (_ :: _ :: _) -> Error `Invalid_result_arity
    end
    | Array_new_default id ->
      let id = find_type id in
      Ok (Array_new_default id)
    | Array_set id ->
      let id = find_type id in
      Ok (Array_set id)
    | Array_get id ->
      let id = find_type id in
      Ok (Array_set id)
    | Ref_null heap_type ->
      let+ t = Binary_types.convert_heap_type None heap_type in
      Ref_null t
    | Br_on_cast (i, t1, t2) ->
      let i = find_type i in
      let* t1 = Binary_types.convert_ref_type None t1 in
      let+ t2 = Binary_types.convert_ref_type None t2 in
      Br_on_cast (i, t1, t2)
    | Br_on_cast_fail (i, null, ht) ->
      let i = find_type i in
      let+ ht = Binary_types.convert_heap_type None ht in
      Br_on_cast_fail (i, null, ht)
    | Struct_new_default i ->
      let i = find_type i in
      Ok (Struct_new_default i)
    | Ref_cast (null, ht) ->
      let+ ht = Binary_types.convert_heap_type None ht in
      Ref_cast (null, ht)
    | Ref_test (null, ht) ->
      let+ ht = Binary_types.convert_heap_type None ht in
      Ref_test (null, ht)
    | ( I_unop _ | I_binop _ | I_testop _ | I_relop _ | F_unop _ | F_relop _
      | I32_wrap_i64 | F_reinterpret_i _ | I_reinterpret_f _ | I64_extend_i32 _
      | I64_extend32_s | F32_demote_f64 | I_extend8_s _ | I_extend16_s _
      | F64_promote_f32 | F_convert_i _ | I_trunc_f _ | I_trunc_sat_f _
      | Ref_is_null | F_binop _ | F32_const _ | F64_const _ | I32_const _
      | I64_const _ | Unreachable | Drop | Nop | Return | Ref_i31 | I31_get_s
      | I31_get_u | Array_len | Ref_as_non_null | Extern_externalize
      | Extern_internalize | Ref_eq | I_load8 _ | I_store8 _ | I_load16 _
      | I_store16 _ | I64_load32 _ | I64_store32 _ | I_load _ | F_load _
      | F_store _ | I_store _ | Memory_copy | Memory_size | Memory_fill
      | Memory_grow ) as i ->
      Ok i
    | ( Array_new_data _ | Array_new _ | Array_new_elem _ | Array_new_fixed _
      | Array_get_u _ | Struct_get _ | Struct_get_s _ | Struct_set _
      | Struct_new _ | Br_on_non_null _ | Br_on_null _ ) as _i ->
      assert false
  and expr (e : text expr) (loop_count, block_ids) : binary expr Result.t =
    list_map (fun i -> body (loop_count, block_ids) i) e
  in
  expr iexpr (0, [])

let rewrite_global (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (global : Text.global) : Binary.global Result.t =
  let* init = rewrite_expr typemap modul [] global.init in
  let mut, val_type = global.typ in
  let+ val_type = Binary_types.convert_val_type None val_type in
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
      let (Raw indice) = find modul.table id in
      let+ expr = rewrite_expr typemap modul [] expr in
      Binary.Elem_active (Some indice, expr)
  in
  let* init = list_map (rewrite_expr typemap modul []) elem.init in
  let+ typ = Binary_types.convert_ref_type None elem.typ in
  { Binary.init; mode; id = elem.id; typ }

let rewrite_data (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  (data : Text.data) : Binary.data Result.t =
  let+ mode =
    match data.mode with
    | Data_passive -> Ok Binary.Data_passive
    | Data_active (None, _expr) -> assert false
    | Data_active (Some indice, expr) ->
      let (Raw indice) = find_memory modul indice in
      let+ expr = rewrite_expr typemap modul [] expr in
      Binary.Data_active (indice, expr)
  in
  { Binary.mode; id = data.id; init = data.init }

let rewrite_export named (exports : Grouped.opt_export list) :
  Binary.export list =
  List.map
    (fun { Grouped.name; id } ->
      let (Raw id) = find named id in
      { Binary.name; id } )
    exports

let rewrite_exports (modul : Assigned.t) (exports : Grouped.opt_exports) :
  Binary.exports =
  let global = rewrite_export modul.global exports.global in
  let mem = rewrite_export modul.mem exports.mem in
  let table = rewrite_export modul.table exports.table in
  let func = rewrite_export modul.func exports.func in
  { Binary.global; mem; table; func }

let rewrite_func (typemap : binary indice TypeMap.t) (modul : Assigned.t)
  ({ id; type_f; locals; body; _ } : text func) : binary func Result.t =
  let* (Bt_raw (_, (params, _)) as type_f) =
    rewrite_block_type typemap modul type_f
  in
  let* locals = list_map (Binary_types.convert_param None) locals in
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
  { named with Named.values }

let rewrite_types (_modul : Assigned.t) (t : binary str_type) :
  binary rec_type Result.t =
  (* TODO: the input `t` should actually be a `binary rec_type` *)
  let t = [ (None, (Final, [], t)) ] in
  Ok t

let rec rewrite_term ~(binder_list : string option list) ~(modul : Binary.modul)
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
    | Text id -> find_raw_indice (`Unknown_binder ind) 0 id binder_list
  in

  let find_param (func_param_list : string option list) (ind : text indice) :
    binary indice Result.t =
    match ind with
    | Raw id -> Ok (Raw id)
    | Text id -> find_raw_indice (`Unknown_param ind) 0 id func_param_list
  in

  let find_global (modul : Binary.modul) (ind : text indice) :
    binary indice Result.t =
    match ind with
    | Raw id -> Ok (Raw id)
    | Text id -> (
      match String_map.find_opt id modul.global.named with
      | None -> Error (`Unknown_global ind)
      | Some i -> Ok (Raw i) )
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
      , find_global modul ind )
    with
    | Ok ind, _, _ -> Ok (BinderVar ind)
    | _, Ok ind, _ -> Ok (ParamVar ind)
    | _, _, Ok ind -> Ok (GlobalVar ind)
    | _, _, _ -> Error (`Unknown_variable ind) )
  | ParamVar ind ->
    let+ ind = find_param func_param_list ind in
    ParamVar ind
  | GlobalVar ind ->
    let+ ind = find_global modul ind in
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
  | Result -> Ok Result

let rec rewrite_prop ~(binder_list : string option list) ~(modul : Binary.modul)
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

let rewrite_contract (modul : Binary.modul) :
  text Contract.t -> binary Contract.t Result.t =
 fun { Contract.funcid; preconditions; postconditions } ->
  let* func = get (`Unknown_func funcid) modul.func funcid in
  let funcid = Raw (Indexed.get_index func) in
  let func_param_list =
    let (Bt_raw (_, (params, _))) =
      match Indexed.get func with
      | Local { type_f; _ } -> type_f
      | Imported { desc; _ } -> desc
    in
    List.map fst params
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

let rewrite_annot (modul : Binary.modul) :
  text Annot.annot -> Binary.custom Result.t = function
  | Contract contract ->
    let+ contract = rewrite_contract modul contract in
    Binary.From_annot (Contract contract)
  | Annot annot -> ok @@ Binary.From_annot (Annot annot)

let rewrite_annots (modul : Binary.modul) :
  text Annot.annot list -> Binary.custom list Result.t =
  list_map (rewrite_annot modul)

let modul (modul : Assigned.t) : Binary.modul Result.t =
  Log.debug0 "rewriting    ...@\n";
  let typemap = typemap modul.typ in
  let* global =
    let+ { Named.named; values } =
      rewrite_named
        (rewrite_runtime (rewrite_global typemap modul) ok)
        modul.global
    in
    let values = List.rev values in
    { Named.named; values }
  in
  let* elem = rewrite_named (rewrite_elem typemap modul) modul.elem in
  let* data = rewrite_named (rewrite_data typemap modul) modul.data in
  let exports = rewrite_exports modul modul.exports in
  let* func =
    let import = rewrite_import (rewrite_block_type typemap modul) in
    let runtime = rewrite_runtime (rewrite_func typemap modul) import in
    rewrite_named runtime modul.func
  in
  let* types = rewrite_named (rewrite_types modul) modul.typ in
  let* start =
    match modul.start with
    | None -> None
    | Some id ->
      let (Raw id) = find func id in
      Some id
  in

  let id = modul.id in
  let mem = Named.to_array modul.mem in
  let table = Named.to_array modul.table in
  let types = Named.to_array types in
  let global = Named.to_array global in
  let elem = Named.to_array elem in
  let data = Named.to_array data in
  let func = Named.to_array func in

  let modul_without_annots : Binary.modul =
    { id; mem; table; types; global; elem; data; exports; func; start; custom = [] }
  in

  let+ custom = rewrite_annots modul_without_annots modul.annots in

  let modul : Binary.modul = { modul_without_annots with custom } in
  modul
