(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module TypeMap = Map.Make (struct
  type t = Binary.func_type

  let compare (x : t) (y : t) = Binary.compare_func_type x y
end)

let typemap (types : Binary.func_type Named.t) =
  Named.fold (fun idx typ acc -> TypeMap.add typ idx acc) types TypeMap.empty

let rewrite_num_type : Text.num_type -> Binary.num_type = function
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128

let rewrite_heap_type : Text.heap_type -> Binary.heap_type = function
  | Func_ht -> Binary.Func_ht
  | Extern_ht -> Binary.Extern_ht

let rewrite_nullable (nullable : Text.nullable) : Binary.nullable =
  match nullable with No_null -> No_null | Null -> Null

let rewrite_ref_type ((nullable, heap_type) : Text.ref_type) : Binary.ref_type =
  let nullable = rewrite_nullable nullable in
  let heap_type = rewrite_heap_type heap_type in
  (nullable, heap_type)

let rewrite_val_type : Text.val_type -> Binary.val_type = function
  | Num_type t -> Num_type (rewrite_num_type t)
  | Ref_type t -> Ref_type (rewrite_ref_type t)

let rewrite_param ((name, t) : Text.param) : Binary.param =
  (name, rewrite_val_type t)

let rewrite_param_type (pt : Text.param_type) : Binary.param_type =
  List.map rewrite_param pt

let rewrite_result_type (rt : Text.result_type) : Binary.result_type =
  List.map rewrite_val_type rt

let rewrite_func_type ((pt, rt) : Text.func_type) : Binary.func_type =
  let pt = rewrite_param_type pt in
  let rt = rewrite_result_type rt in
  (pt, rt)

let rewrite_nn : Text.nn -> Binary.nn = function S32 -> S32 | S64 -> S64

let rewrite_ishape : Text.ishape -> Binary.ishape = function
  | I8x16 -> I8x16
  | I16x8 -> I16x8
  | I32x4 -> I32x4
  | I64x2 -> I64x2

let rewrite_sx : Text.sx -> Binary.sx = function U -> U | S -> S

let rewrite_iunop : Text.iunop -> Binary.iunop = function
  | Clz -> Clz
  | Ctz -> Ctz
  | Popcnt -> Popcnt

let rewrite_funop : Text.funop -> Binary.funop = function
  | Abs -> Abs
  | Neg -> Neg
  | Sqrt -> Sqrt
  | Ceil -> Ceil
  | Floor -> Floor
  | Trunc -> Trunc
  | Nearest -> Nearest

let rewrite_vibinop : Text.vibinop -> Binary.vibinop = function
  | Add -> Add
  | Sub -> Sub

let rewrite_ibinop : Text.ibinop -> Binary.ibinop = function
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div sx -> Div (rewrite_sx sx)
  | Rem sx -> Rem (rewrite_sx sx)
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Shl -> Shl
  | Shr sx -> Shr (rewrite_sx sx)
  | Rotl -> Rotl
  | Rotr -> Rotr

let rewrite_fbinop : Text.fbinop -> Binary.fbinop = function
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Min -> Min
  | Max -> Max
  | Copysign -> Copysign

let rewrite_itestop : Text.itestop -> Binary.itestop = function Eqz -> Eqz

let rewrite_irelop : Text.irelop -> Binary.irelop = function
  | Eq -> Eq
  | Ne -> Ne
  | Lt sx -> Lt (rewrite_sx sx)
  | Gt sx -> Gt (rewrite_sx sx)
  | Le sx -> Le (rewrite_sx sx)
  | Ge sx -> Ge (rewrite_sx sx)

let rewrite_frelop : Text.frelop -> Binary.frelop = function
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge

let rewrite_memarg : Text.memarg -> Binary.memarg = function
  | { offset; align } -> { offset; align }

let rewrite_limits : Text.limits -> Binary.limits = function
  | { min; max } -> { min; max }

let rewrite_mem ((name, limits) : Text.Mem.t) : Binary.Mem.t =
  (name, rewrite_limits limits)

let rewrite_block_type (typemap : Binary.indice TypeMap.t) (modul : Assigned.t)
  (block_type : Text.block_type) : Binary.block_type Result.t =
  match block_type with
  | Bt_ind id -> begin
    let* idx = Assigned.find_type modul id in
    match Named.get_at modul.typ idx with
    | None -> Error (`Unknown_type id)
    | Some v ->
      let t' = rewrite_func_type v in
      Ok (Binary.Bt_raw (Some idx, t'))
  end
  | Bt_raw (_, func_type) ->
    let func_type = rewrite_func_type func_type in
    let idx =
      match TypeMap.find_opt func_type typemap with
      | None -> assert false
      | Some idx -> idx
    in
    Ok (Binary.Bt_raw (Some idx, func_type))

let rewrite_mut = function Text.Const -> Binary.Const | Var -> Var

let rewrite_global_type (mut, t) =
  let mut = rewrite_mut mut in
  let t = rewrite_val_type t in
  (mut, t)

let rewrite_table_type ((limits, ref_type) : Text.Table.Type.t) :
  Binary.Table.Type.t =
  let limits = rewrite_limits limits in
  let ref_type = rewrite_ref_type ref_type in
  (limits, ref_type)

let rewrite_table ((name, typ) : Text.Table.t) : Binary.Table.t =
  let typ = rewrite_table_type typ in
  (name, typ)

let rewrite_expr (typemap : Binary.indice TypeMap.t) (modul : Assigned.t)
  (locals : Binary.param list) (iexpr : Text.expr Annotated.t) :
  Binary.expr Annotated.t Result.t =
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
      let+ bt = rewrite_block_type typemap modul bt in
      Some bt
    | None -> Ok None
  in

  let seen_locals = Hashtbl.create 64 in

  (* Fill locals *)
  let* (_ : int) =
    list_fold_left
      (fun next_free_int ((name, _type) : Binary.param) ->
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
      let+ id = Assigned.find_func modul id in
      Binary.Call id
    | Return_call id ->
      let+ id = Assigned.find_func modul id in
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
      let* tbl_i = Assigned.find_table modul tbl_i in
      let+ bt = rewrite_block_type typemap modul bt in
      Binary.Call_indirect (tbl_i, bt)
    | Return_call_indirect (tbl_i, bt) ->
      let* tbl_i = Assigned.find_table modul tbl_i in
      let+ bt = rewrite_block_type typemap modul bt in
      Binary.Return_call_indirect (tbl_i, bt)
    | Call_ref t ->
      let+ t = Assigned.find_type modul t in
      Binary.Call_ref t
    | Return_call_ref bt ->
      let+ bt = rewrite_block_type typemap modul bt in
      Binary.Return_call_ref bt
    | Global_set id ->
      let+ idx = Assigned.find_global modul id in
      Binary.Global_set idx
    | Global_get id ->
      let+ idx = Assigned.find_global modul id in
      Binary.Global_get idx
    | Ref_func id ->
      let+ id = Assigned.find_func modul id in
      Binary.Ref_func id
    | Table_size id ->
      let+ id = Assigned.find_table modul id in
      Binary.Table_size id
    | Table_get id ->
      let+ id = Assigned.find_table modul id in
      Binary.Table_get id
    | Table_set id ->
      let+ id = Assigned.find_table modul id in
      Binary.Table_set id
    | Table_grow id ->
      let+ id = Assigned.find_table modul id in
      Binary.Table_grow id
    | Table_init (i, i') ->
      let* table = Assigned.find_table modul i in
      let+ elem = Assigned.find_elem modul i' in
      Binary.Table_init (table, elem)
    | Table_fill id ->
      let+ id = Assigned.find_table modul id in
      Binary.Table_fill id
    | Table_copy (i, i') ->
      let* table = Assigned.find_table modul i in
      let+ table' = Assigned.find_table modul i' in
      Binary.Table_copy (table, table')
    | Memory_init id ->
      let+ id = Assigned.find_data modul id in
      Binary.Memory_init id
    | Data_drop id ->
      let+ id = Assigned.find_data modul id in
      Binary.Data_drop id
    | Elem_drop id ->
      let+ id = Assigned.find_elem modul id in
      Binary.Elem_drop id
    | Select typ -> begin
      match typ with
      | None -> Ok (Binary.Select None)
      | Some [ t ] -> Ok (Binary.Select (Some [ rewrite_val_type t ]))
      | Some [] | Some (_ :: _ :: _) -> Error `Invalid_result_arity
    end
    | I_unop (nn, op) ->
      let nn = rewrite_nn nn in
      let op = rewrite_iunop op in
      Ok (Binary.I_unop (nn, op))
    | I_binop (nn, op) ->
      let nn = rewrite_nn nn in
      let op = rewrite_ibinop op in
      Ok (I_binop (nn, op))
    | I_testop (nn, op) ->
      let nn = rewrite_nn nn in
      let op = rewrite_itestop op in
      Ok (Binary.I_testop (nn, op))
    | I_relop (nn, op) ->
      let nn = rewrite_nn nn in
      let op = rewrite_irelop op in
      Ok (I_relop (nn, op))
    | F_unop (nn, op) ->
      let nn = rewrite_nn nn in
      let op = rewrite_funop op in
      Ok (Binary.F_unop (nn, op))
    | F_relop (nn, op) ->
      let nn = rewrite_nn nn in
      let op = rewrite_frelop op in
      Ok (F_relop (nn, op))
    | I32_wrap_i64 -> Ok Binary.I32_wrap_i64
    | F_reinterpret_i (nn1, nn2) ->
      let nn1 = rewrite_nn nn1 in
      let nn2 = rewrite_nn nn2 in
      Ok (F_reinterpret_i (nn1, nn2))
    | I_reinterpret_f (nn1, nn2) ->
      let nn1 = rewrite_nn nn1 in
      let nn2 = rewrite_nn nn2 in
      Ok (I_reinterpret_f (nn1, nn2))
    | I64_extend_i32 sx ->
      let sx = rewrite_sx sx in
      Ok (Binary.I64_extend_i32 sx)
    | I64_extend32_s -> Ok Binary.I64_extend32_s
    | F32_demote_f64 -> Ok Binary.F32_demote_f64
    | I_extend8_s nn ->
      let nn = rewrite_nn nn in
      Ok (I_extend8_s nn)
    | I_extend16_s nn ->
      let nn = rewrite_nn nn in
      Ok (I_extend16_s nn)
    | F64_promote_f32 -> Ok Binary.F64_promote_f32
    | F_convert_i (nn1, nn2, sx) ->
      let nn1 = rewrite_nn nn1 in
      let nn2 = rewrite_nn nn2 in
      let sx = rewrite_sx sx in
      Ok (Binary.F_convert_i (nn1, nn2, sx))
    | I_trunc_f (nn1, nn2, sx) ->
      let nn1 = rewrite_nn nn1 in
      let nn2 = rewrite_nn nn2 in
      let sx = rewrite_sx sx in
      Ok (Binary.I_trunc_f (nn1, nn2, sx))
    | I_trunc_sat_f (nn1, nn2, sx) ->
      let nn1 = rewrite_nn nn1 in
      let nn2 = rewrite_nn nn2 in
      let sx = rewrite_sx sx in
      Ok (Binary.I_trunc_sat_f (nn1, nn2, sx))
    | Ref_is_null -> Ok Binary.Ref_is_null
    | F_binop (nn, op) ->
      let nn = rewrite_nn nn in
      let op = rewrite_fbinop op in
      Ok (Binary.F_binop (nn, op))
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
    | I_load8 (nn, sx, memarg) ->
      let nn = rewrite_nn nn in
      let sx = rewrite_sx sx in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I_load8 (nn, sx, memarg))
    | I_store8 (nn, memarg) ->
      let nn = rewrite_nn nn in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I_store8 (nn, memarg))
    | I_load16 (nn, sx, memarg) ->
      let nn = rewrite_nn nn in
      let sx = rewrite_sx sx in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I_load16 (nn, sx, memarg))
    | I_store16 (nn, memarg) ->
      let nn = rewrite_nn nn in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I_store16 (nn, memarg))
    | I64_load32 (sx, memarg) ->
      let sx = rewrite_sx sx in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I64_load32 (sx, memarg))
    | I64_store32 memarg ->
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I64_store32 memarg)
    | I_load (nn, memarg) ->
      let nn = rewrite_nn nn in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I_load (nn, memarg))
    | F_load (nn, memarg) ->
      let nn = rewrite_nn nn in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.F_load (nn, memarg))
    | F_store (nn, memarg) ->
      let nn = rewrite_nn nn in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.F_store (nn, memarg))
    | I_store (nn, memarg) ->
      let nn = rewrite_nn nn in
      let memarg = rewrite_memarg memarg in
      Ok (Binary.I_store (nn, memarg))
    | Memory_copy -> Ok Binary.Memory_copy
    | Memory_size -> Ok Binary.Memory_size
    | Memory_fill -> Ok Binary.Memory_fill
    | Memory_grow -> Ok Binary.Memory_grow
    | V_ibinop (shape, op) ->
      let shape = rewrite_ishape shape in
      let op = rewrite_vibinop op in
      Ok (Binary.V_ibinop (shape, op))
    | Ref_null t ->
      let t = rewrite_heap_type t in
      Ok (Binary.Ref_null t)
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

let rewrite_global (typemap : Binary.indice TypeMap.t) (modul : Assigned.t)
  (global : Text.Global.t) : Binary.Global.t Result.t =
  let+ init = rewrite_expr typemap modul [] global.init in
  let typ = rewrite_global_type global.typ in
  { Binary.Global.id = global.id; init; typ }

let rewrite_elem (typemap : Binary.indice TypeMap.t) (modul : Assigned.t)
  (elem : Text.Elem.t) : Binary.Elem.t Result.t =
  let* (mode : Binary.Elem.Mode.t) =
    match elem.mode with
    | Declarative -> Ok Binary.Elem.Mode.Declarative
    | Passive -> Ok Passive
    | Active (None, _expr) -> assert false
    | Active (Some id, expr) ->
      let* indice = Assigned.find_table modul id in
      let+ expr = rewrite_expr typemap modul [] expr in
      Binary.Elem.Mode.Active (Some indice, expr)
  in
  let+ init = list_map (rewrite_expr typemap modul []) elem.init in
  let typ = rewrite_ref_type elem.typ in
  { Binary.Elem.init; mode; id = elem.id; typ }

let rewrite_data (typemap : Binary.indice TypeMap.t) (modul : Assigned.t)
  (data : Text.Data.t) : Binary.Data.t Result.t =
  let+ mode =
    match data.mode with
    | Passive -> Ok Binary.Data.Mode.Passive
    | Active (None, _expr) -> assert false
    | Active (Some indice, expr) ->
      let* indice = Assigned.find_memory modul indice in
      let+ expr = rewrite_expr typemap modul [] expr in
      Binary.Data.Mode.Active (indice, expr)
  in
  { Binary.Data.mode; id = data.id; init = data.init }

let rewrite_export named (exports : Grouped.opt_export Array.t) :
  Binary.Export.t Array.t Result.t =
  array_map
    (fun { Grouped.name; id } ->
      let+ id =
        match id with
        | Text.Raw i -> Ok i
        | Text name -> (
          match Named.get_by_name named name with
          | None -> Error (`Unknown_export id)
          | Some i -> Ok i )
      in

      { Binary.Export.name; id } )
    exports

let rewrite_exports (modul : Assigned.t) : Binary.Module.Exports.t Result.t =
  let* global = rewrite_export modul.global modul.global_exports in
  let* mem = rewrite_export modul.mem modul.mem_exports in
  let* table = rewrite_export modul.table modul.table_exports in
  let+ func = rewrite_export modul.func modul.func_exports in
  { Binary.Module.Exports.global; mem; table; func }

let rewrite_func (typemap : Binary.indice TypeMap.t) (modul : Assigned.t)
  ({ id; type_f; locals; body; _ } : Text.Func.t) : Binary.Func.t Result.t =
  let* (Bt_raw (_, (params, _)) as type_f) =
    rewrite_block_type typemap modul type_f
  in
  let locals = List.map rewrite_param locals in
  let+ body = rewrite_expr typemap modul (params @ locals) body in
  { Binary.Func.body; type_f; id; locals }

let rewrite_types (t : Binary.func_type) : Binary.Typedef.t Result.t =
  Ok (None, t)

let modul (modul : Assigned.t) : Binary.Module.t Result.t =
  Log.debug (fun m -> m "rewriting    ...");
  let modul_typ = Named.map rewrite_func_type modul.typ in
  let typemap = typemap modul_typ in
  let* global =
    Named.monadic_map
      (Origin.monadic_map ~f_local:(rewrite_global typemap modul)
         ~f_imported:(fun x -> Ok (rewrite_global_type x) ) )
      modul.global
  in
  let* elem = Named.monadic_map (rewrite_elem typemap modul) modul.elem in
  let* data = Named.monadic_map (rewrite_data typemap modul) modul.data in
  let* exports = rewrite_exports modul in
  let* func =
    let f_imported = rewrite_block_type typemap modul in
    let f_local = rewrite_func typemap modul in
    let runtime = Origin.monadic_map ~f_local ~f_imported in
    Named.monadic_map runtime modul.func
  in
  let* types = Named.monadic_map rewrite_types modul_typ in
  let mem =
    let runtime = Origin.map ~f_local:rewrite_mem ~f_imported:rewrite_limits in
    Named.map runtime modul.mem
  in
  let table =
    let runtime =
      Origin.map ~f_local:rewrite_table ~f_imported:rewrite_table_type
    in
    Named.map runtime modul.table
  in
  let+ start =
    match modul.start with
    | None -> Ok None
    | Some id ->
      let+ id = Assigned.find_func modul id in
      Some id
  in
  let custom = [] in

  let id = modul.id in
  let mem = Named.to_array mem in
  let table = Named.to_array table in
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
