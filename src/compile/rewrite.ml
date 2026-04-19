(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let rewrite_heap_type (assigned : Assigned.t) (ht : Text.heap_type) :
  Binary.heap_type Result.t =
  match ht with
  | Text.TypeUse id ->
    let* id = Assigned.find_type assigned id in
    Ok (Binary.TypeUse id)
  | Any_ht -> Ok Any_ht
  | Eq_ht -> Ok Eq_ht
  | I31_ht -> Ok I31_ht
  | Struct_ht -> Ok Struct_ht
  | Array_ht -> Ok Array_ht
  | None_ht -> Ok None_ht
  | Func_ht -> Ok Func_ht
  | NoFunc_ht -> Ok NoFunc_ht
  | Exn_ht -> Ok Exn_ht
  | NoExn_ht -> Ok NoExn_ht
  | Extern_ht -> Ok Extern_ht
  | NoExtern_ht -> Ok NoExtern_ht

let rewrite_val_type (assigned : Assigned.t) (vt : Text.val_type) :
  Binary.val_type Result.t =
  match vt with
  | Text.Ref_type (n, ht) ->
    let* ht = rewrite_heap_type assigned ht in
    Ok (Binary.Ref_type (n, ht))
  | Num_type nt -> Ok (Num_type nt)

let rewrite_func_type (assigned : Assigned.t) ((param, res) : Text.func_type) :
  Binary.func_type Result.t =
  let* param' =
    list_map
      (fun (n, vt) ->
        let* vt = rewrite_val_type assigned vt in
        Ok (n, vt) )
      param
  in
  let* res' = list_map (rewrite_val_type assigned) res in
  Ok (param', res')

let rewrite_block_type (assigned : Assigned.t) (block_type : Text.block_type) :
  (Text.param list * Binary.block_type) Result.t =
  match block_type with
  | Bt_ind id ->
    let* idx = Assigned.find_type assigned id in
    let* ((params, _) as t) =
      match Assigned.get_type assigned idx with
      | None -> Error (`Unknown_type id)
      | Some v -> Ok v
    in
    let* t = rewrite_func_type assigned t in
    Ok (params, Binary.Bt_raw (Some idx, t))
  | Bt_raw (_, ((params, _) as func_type)) ->
    let idx = Assigned.find_raw_type assigned func_type in
    let* func_type = rewrite_func_type assigned func_type in
    Ok (params, Binary.Bt_raw (Some idx, func_type))

let rewrite_memarg ({ offset; align } : Text.memarg) : Binary.memarg Result.t =
  let* offset =
    match offset with
    | None -> Ok Int64.zero
    | Some offset -> (
      match Int64.of_string offset with
      | None -> Error (`Msg "offset")
      | Some n -> Ok n )
  in
  let+ align =
    match align with
    | None -> Ok Int32.zero
    | Some align -> (
      match Int32.of_string align with
      | None -> Error `Alignment_too_large
      | Some n ->
        if Int32.eq n 0l || Int32.(ne (logand n (sub n 1l)) 0l) then
          Error `Alignment_too_large
        else Ok (Int32.div n 2l) )
  in
  Binary.{ offset; align }

let rewrite_i32_instr assigned : Text.i32_instr -> Binary.i32_instr Result.t =
  function
  | Const i -> Ok (Binary.Const i : Binary.i32_instr)
  | Clz -> Ok Clz
  | Ctz -> Ok Ctz
  | Popcnt -> Ok Popcnt
  | Add -> Ok Add
  | Sub -> Ok Sub
  | Mul -> Ok Mul
  | Div sx -> Ok (Div sx)
  | Rem sx -> Ok (Rem sx)
  | And -> Ok And
  | Or -> Ok Or
  | Xor -> Ok Xor
  | Shl -> Ok Shl
  | Shr sx -> Ok (Shr sx)
  | Rotl -> Ok Rotl
  | Rotr -> Ok Rotr
  | Eqz -> Ok Eqz
  | Eq -> Ok Eq
  | Ne -> Ok Ne
  | Lt sx -> Ok (Lt sx)
  | Gt sx -> Ok (Gt sx)
  | Le sx -> Ok (Le sx)
  | Ge sx -> Ok (Ge sx)
  | Extend8_s -> Ok Extend8_s
  | Extend16_s -> Ok Extend16_s
  | Wrap_i64 -> Ok Wrap_i64
  | Trunc_f (nn, sx) -> Ok (Trunc_f (nn, sx))
  | Trunc_sat_f (nn, sx) -> Ok (Trunc_sat_f (nn, sx))
  | Reinterpret_f nn -> Ok (Reinterpret_f nn)
  | Load (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load (indice, memarg) : Binary.i32_instr)
  | Load8 (indice, sx, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load8 (indice, sx, memarg) : Binary.i32_instr)
  | Load16 (indice, sx, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load16 (indice, sx, memarg) : Binary.i32_instr)
  | Store (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store (indice, memarg) : Binary.i32_instr)
  | Store8 (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store8 (indice, memarg) : Binary.i32_instr)
  | Store16 (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store16 (indice, memarg) : Binary.i32_instr)

let rewrite_i64_instr assigned : Text.i64_instr -> Binary.i64_instr Result.t =
  function
  | Const i -> Ok (Binary.Const i : Binary.i64_instr)
  | Clz -> Ok Clz
  | Ctz -> Ok Ctz
  | Popcnt -> Ok Popcnt
  | Add -> Ok Add
  | Sub -> Ok Sub
  | Mul -> Ok Mul
  | Div sx -> Ok (Div sx)
  | Rem sx -> Ok (Rem sx)
  | And -> Ok And
  | Or -> Ok Or
  | Xor -> Ok Xor
  | Shl -> Ok Shl
  | Shr sx -> Ok (Shr sx)
  | Rotl -> Ok Rotl
  | Rotr -> Ok Rotr
  | Eqz -> Ok Eqz
  | Eq -> Ok Eq
  | Ne -> Ok Ne
  | Lt sx -> Ok (Lt sx)
  | Gt sx -> Ok (Gt sx)
  | Le sx -> Ok (Le sx)
  | Ge sx -> Ok (Ge sx)
  | Extend8_s -> Ok Extend8_s
  | Extend16_s -> Ok Extend16_s
  | Extend32_s -> Ok Extend32_s
  | Extend_i32 sx -> Ok (Extend_i32 sx)
  | Trunc_f (nn, sx) -> Ok (Trunc_f (nn, sx))
  | Trunc_sat_f (nn, sx) -> Ok (Trunc_sat_f (nn, sx))
  | Reinterpret_f nn -> Ok (Reinterpret_f nn)
  | Load (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load (indice, memarg) : Binary.i64_instr)
  | Load8 (indice, sx, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load8 (indice, sx, memarg) : Binary.i64_instr)
  | Load16 (indice, sx, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load16 (indice, sx, memarg) : Binary.i64_instr)
  | Load32 (indice, sx, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load32 (indice, sx, memarg) : Binary.i64_instr)
  | Store (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store (indice, memarg) : Binary.i64_instr)
  | Store8 (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store8 (indice, memarg) : Binary.i64_instr)
  | Store16 (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store16 (indice, memarg) : Binary.i64_instr)
  | Store32 (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store32 (indice, memarg) : Binary.i64_instr)

let rewrite_f32_instr assigned : Text.f32_instr -> Binary.f32_instr Result.t =
  function
  | Const f -> Ok (Const f : Binary.f32_instr)
  | Abs -> Ok Abs
  | Neg -> Ok Neg
  | Sqrt -> Ok Sqrt
  | Ceil -> Ok Ceil
  | Floor -> Ok Floor
  | Trunc -> Ok Trunc
  | Nearest -> Ok Nearest
  | Add -> Ok Add
  | Sub -> Ok Sub
  | Mul -> Ok Mul
  | Div -> Ok Div
  | Min -> Ok Min
  | Max -> Ok Max
  | Copysign -> Ok Copysign
  | Eq -> Ok Eq
  | Ne -> Ok Ne
  | Lt -> Ok Lt
  | Gt -> Ok Gt
  | Le -> Ok Le
  | Ge -> Ok Ge
  | Demote_f64 -> Ok Demote_f64
  | Convert_i (nn, sx) -> Ok (Convert_i (nn, sx))
  | Reinterpret_i nn -> Ok (Reinterpret_i nn)
  | Load (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load (indice, memarg) : Binary.f32_instr)
  | Store (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store (indice, memarg) : Binary.f32_instr)

let rewrite_f64_instr assigned : Text.f64_instr -> Binary.f64_instr Result.t =
  function
  | Const f -> Ok (Const f : Binary.f64_instr)
  | Abs -> Ok Abs
  | Neg -> Ok Neg
  | Sqrt -> Ok Sqrt
  | Ceil -> Ok Ceil
  | Floor -> Ok Floor
  | Trunc -> Ok Trunc
  | Nearest -> Ok Nearest
  | Add -> Ok Add
  | Sub -> Ok Sub
  | Mul -> Ok Mul
  | Div -> Ok Div
  | Min -> Ok Min
  | Max -> Ok Max
  | Copysign -> Ok Copysign
  | Eq -> Ok Eq
  | Ne -> Ok Ne
  | Lt -> Ok Lt
  | Gt -> Ok Gt
  | Le -> Ok Le
  | Ge -> Ok Ge
  | Promote_f32 -> Ok Promote_f32
  | Convert_i (nn, sx) -> Ok (Convert_i (nn, sx))
  | Reinterpret_i nn -> Ok (Reinterpret_i nn)
  | Load (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Load (indice, memarg) : Binary.f64_instr)
  | Store (indice, memarg) ->
    let* memarg = rewrite_memarg memarg in
    let+ indice = Assigned.find_memory assigned indice in
    (Store (indice, memarg) : Binary.f64_instr)

let rewrite_ref_instr assigned : Text.ref_instr -> Binary.ref_instr Result.t =
  function
  | Null heap_type ->
    let+ heap_type = rewrite_heap_type assigned heap_type in
    Binary.Null heap_type
  | Is_null -> Ok Binary.Is_null
  | As_non_null -> Ok Binary.As_non_null
  | Func indice ->
    let+ indice = Assigned.find_func assigned indice in
    Binary.Func indice

let rewrite_global_instr assigned :
  Text.global_instr -> Binary.global_instr Result.t = function
  | Set id ->
    let+ idx = Assigned.find_global assigned id in
    (Set idx : Binary.global_instr)
  | Get id ->
    let+ idx = Assigned.find_global assigned id in
    (Get idx : Binary.global_instr)

let rewrite_table_instr assigned :
  Text.table_instr -> Binary.table_instr Result.t = function
  | Size indice ->
    let+ indice = Assigned.find_table assigned indice in
    (Size indice : Binary.table_instr)
  | Get indice ->
    let+ indice = Assigned.find_table assigned indice in
    Binary.Get indice
  | Set indice ->
    let+ indice = Assigned.find_table assigned indice in
    Binary.Set indice
  | Grow indice ->
    let+ indice = Assigned.find_table assigned indice in
    (Grow indice : Binary.table_instr)
  | Init (indice, indice') ->
    let* table = Assigned.find_table assigned indice in
    let+ elem = Assigned.find_elem assigned indice' in
    (Init (table, elem) : Binary.table_instr)
  | Fill indice ->
    let+ indice = Assigned.find_table assigned indice in
    (Fill indice : Binary.table_instr)
  | Copy (indice, indice') ->
    let* table = Assigned.find_table assigned indice in
    let+ table' = Assigned.find_table assigned indice' in
    (Copy (table, table') : Binary.table_instr)

let rewrite_elem_instr assigned : Text.elem_instr -> Binary.elem_instr Result.t
    = function
  | Drop id ->
    let+ id = Assigned.find_elem assigned id in
    (Drop id : Binary.elem_instr)

let rewrite_memory_instr assigned :
  Text.memory_instr -> Binary.memory_instr Result.t = function
  | Init (mem_indice, data_indice) ->
    let* mem_indice = Assigned.find_memory assigned mem_indice in
    let+ data_indice = Assigned.find_data assigned data_indice in
    Binary.Init (mem_indice, data_indice)
  | Copy (indice1, indice2) ->
    let* indice1 = Assigned.find_memory assigned indice1 in
    let+ indice2 = Assigned.find_memory assigned indice2 in
    Binary.Copy (indice1, indice2)
  | Size indice ->
    let+ indice = Assigned.find_memory assigned indice in
    Binary.Size indice
  | Fill indice ->
    let+ indice = Assigned.find_memory assigned indice in
    Binary.Fill indice
  | Grow indice ->
    let+ indice = Assigned.find_memory assigned indice in
    Binary.Grow indice

let rewrite_data_instr assigned : Text.data_instr -> Binary.data_instr Result.t
    = function
  | Drop id ->
    let+ id = Assigned.find_data assigned id in
    (Drop id : Binary.data_instr)

let rewrite_expr (assigned : Assigned.t) (locals : Text.param list)
  (iexpr : Text.expr) : Binary.expr Annotated.t Result.t =
  (* block_ids handling *)
  let block_id_to_raw (loop_count, block_ids) id =
    let* id =
      match id with
      | Text.Text id ->
        begin match
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
      let+ _, bt = rewrite_block_type assigned bt in
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
      | Some id -> id
      | None -> assert false )
  in

  let rewrite_local_instr : Text.local_instr -> Binary.local_instr = function
    | Set id ->
      let id = find_local id in
      Set id
    | Get id ->
      let id = find_local id in
      Get id
    | Tee id ->
      let id = find_local id in
      Tee id
  in

  let rec rewrite_instr (loop_count, block_ids) :
    Text.instr -> Binary.instr Result.t = function
    | I32 i ->
      let+ i = rewrite_i32_instr assigned i in
      Binary.I32 i
    | I64 i ->
      let+ i = rewrite_i64_instr assigned i in
      Binary.I64 i
    | F32 i ->
      let+ i = rewrite_f32_instr assigned i in
      Binary.F32 i
    | F64 i ->
      let+ i = rewrite_f64_instr assigned i in
      Binary.F64 i
    | V128 i -> Ok (Binary.V128 i)
    | I8x16 i -> Ok (Binary.I8x16 i)
    | I16x8 i -> Ok (Binary.I16x8 i)
    | I32x4 i -> Ok (Binary.I32x4 i)
    | I64x2 i -> Ok (Binary.I64x2 i)
    | Ref i ->
      let+ i = rewrite_ref_instr assigned i in
      Binary.Ref i
    | Local i ->
      let i = rewrite_local_instr i in
      Ok (Binary.Local i)
    | Global i ->
      let+ i = rewrite_global_instr assigned i in
      Binary.Global i
    | Table i ->
      let+ i = rewrite_table_instr assigned i in
      Binary.Table i
    | Elem i ->
      let+ i = rewrite_elem_instr assigned i in
      Binary.Elem i
    | Memory i ->
      let+ i = rewrite_memory_instr assigned i in
      Binary.Memory i
    | Data i ->
      let+ i = rewrite_data_instr assigned i in
      Binary.Data i
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
    | Br_on_null id ->
      let+ id = block_id_to_raw (loop_count, block_ids) id in
      Binary.Br_on_null id
    | Br_on_non_null id ->
      let+ id = block_id_to_raw (loop_count, block_ids) id in
      Binary.Br_on_non_null id
    | Call id ->
      let+ id = Assigned.find_func assigned id in
      Binary.Call id
    | Return_call id ->
      let+ id = Assigned.find_func assigned id in
      Binary.Return_call id
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
      let+ _, bt = rewrite_block_type assigned bt in
      Binary.Call_indirect (tbl_i, bt)
    | Return_call_indirect (tbl_i, bt) ->
      let* tbl_i = Assigned.find_table assigned tbl_i in
      let+ _, bt = rewrite_block_type assigned bt in
      Binary.Return_call_indirect (tbl_i, bt)
    | Call_ref t ->
      let+ t = Assigned.find_type assigned t in
      Binary.Call_ref t
    | Return_call_ref bt ->
      let+ _, bt = rewrite_block_type assigned bt in
      Binary.Return_call_ref bt
    | Select typ ->
      begin match typ with
      | None -> Ok (Binary.Select None)
      | Some [ t ] ->
        let+ t = rewrite_val_type assigned t in
        Binary.Select (Some [ t ])
      | Some [] | Some (_ :: _ :: _) -> Error `Invalid_result_arity
      end
    | Unreachable -> Ok Binary.Unreachable
    | Drop -> Ok Binary.Drop
    | Nop -> Ok Binary.Nop
    | Return -> Ok Binary.Return
    | Ref_i31 -> Ok Ref_i31
    | Ref_eq -> Ok Ref_eq
    | Ref_test (n, ht) ->
      let+ ht = rewrite_heap_type assigned ht in
      Binary.Ref_test (n, ht)
    | Ref_cast (n, ht) ->
      let+ ht = rewrite_heap_type assigned ht in
      Binary.Ref_cast (n, ht)
    | I31_get_s -> Ok I31_get_s
    | I31_get_u -> Ok I31_get_u
    | Array_len -> Ok Array_len
    | Any_convert_extern -> Ok Any_convert_extern
    | Extern_convert_any -> Ok Extern_convert_any
    | ( Struct_new _ | Struct_new_default _ | Struct_get _ | Struct_get_s _
      | Struct_get_u _ | Struct_set _ | Array_new _ | Array_new_default _
      | Array_new_fixed _ | Array_new_data _ | Array_new_elem _ | Array_get _
      | Array_get_s _ | Array_get_u _ | Array_set _ | Array_fill _
      | Array_copy _ | Array_init_data _ | Array_init_elem _ ) as instr ->
      Fmt.failwith "Rewrite: unimplemented for the GC instruction %a"
        (Text.pp_instr ~short:true)
        instr
  and expr (e : Text.expr) (loop_count, block_ids) :
    Binary.expr Annotated.t Result.t =
    let+ e =
      list_map
        (fun i ->
          let+ i = rewrite_instr (loop_count, block_ids) i in
          Annotated.dummy i )
        e
    in
    Annotated.dummy e
  in
  expr iexpr (0, [])

let rewrite_table_limits ({ is_i64; min; max } : Text.limits) :
  Binary.Table.Type.limits Result.t =
  if is_i64 then
    let* min =
      try Ok (Int64.of_string_exn min) with Failure _ -> Error `Table_size
    in
    let* max =
      try Ok (Option.map Int64.of_string_exn max)
      with Failure _ -> Error `Table_size
    in
    Ok (Binary.Table.Type.I64 { min; max })
  else
    let* min =
      try Ok (Int32.of_string_exn min) with Failure _ -> Error `Table_size
    in
    let* max =
      try Ok (Option.map Int32.of_string_exn max)
      with Failure _ -> Error `Table_size
    in
    Ok (Binary.Table.Type.I32 { min; max })

let rewrite_table (assigned : Assigned.t)
  ({ id; typ = limits, (null, ht); init } : Text.Table.t) :
  Binary.Table.t Result.t =
  match init with
  | None ->
    let* ht = rewrite_heap_type assigned ht in
    let+ limits = rewrite_table_limits limits in
    { Binary.Table.id; typ = (limits, (null, ht)); init = None }
  | Some e ->
    let* ht = rewrite_heap_type assigned ht in
    let* e = rewrite_expr assigned [] e in
    let+ limits = rewrite_table_limits limits in
    { Binary.Table.id; typ = (limits, (null, ht)); init = Some e }

let rewrite_memory_limits ({ is_i64; min; max } : Text.limits) :
  Binary.Mem.Type.limits Result.t =
  if is_i64 then
    let* min =
      match int_of_string_opt min with
      | Some min -> Ok min
      | None -> Error `Constant_out_of_range
    in
    let* max =
      match max with
      | None -> Ok None
      | Some max -> (
        match int_of_string_opt max with
        | Some max -> Ok (Some max)
        | None -> Error `Constant_out_of_range )
    in
    Ok (Binary.Mem.Type.I64 { min; max })
  else
    let* min =
      try Ok (Int32.of_string_exn min)
      with Failure _ -> Error `Constant_out_of_range
    in
    let* max =
      try Ok (Option.map Int32.of_string_exn max)
      with Failure _ -> Error `Constant_out_of_range
    in
    Ok (Binary.Mem.Type.I32 { min; max })

let rewrite_memory ((id, limits) : Text.Mem.t) : Binary.Mem.t Result.t =
  let+ limits = rewrite_memory_limits limits in
  (id, limits)

let rewrite_global (assigned : Assigned.t) (global : Text.Global.t) :
  Binary.Global.t Result.t =
  let mut, vt = global.typ in
  let* vt = rewrite_val_type assigned vt in
  let+ init = rewrite_expr assigned [] global.init in
  { Binary.Global.id = global.id; init; typ = (mut, vt) }

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
  let* init = list_map (rewrite_expr assigned []) elem.init in
  let nullable, ht = elem.typ in
  let+ ht = rewrite_heap_type assigned ht in
  { Binary.Elem.init
  ; mode
  ; id = elem.id
  ; typ = (nullable, ht)
  ; explicit_typ = elem.explicit_typ
  }

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
  let* func = rewrite_export Assigned.find_func assigned modul.func_exports in
  let+ tag = rewrite_export Assigned.find_tag assigned modul.tag_exports in
  { Binary.Module.Exports.global; mem; table; func; tag }

let rewrite_func (assigned : Assigned.t)
  ({ id; type_f; locals; body; _ } : Text.Func.t) : Binary.Func.t Result.t =
  let* params, type_f = rewrite_block_type assigned type_f in
  let* body = rewrite_expr assigned (params @ locals) body in
  let+ locals : Binary.param_type =
    list_map
      (fun (_n, vt) ->
        let* vt = rewrite_val_type assigned vt in
        Ok (None, vt) )
      locals
  in
  { Binary.Func.body; type_f; id; locals }

let rewrite_tag (assigned : Assigned.t) ({ id; typ } : Text.Tag.t) :
  Binary.Tag.t Result.t =
  let+ _, typ = rewrite_block_type assigned typ in
  Binary.Tag.{ id; typ }

let rewrite_types (assigned : Assigned.t) (ft : Text.func_type) :
  Binary.Typedef.t Result.t =
  let* ft = rewrite_func_type assigned ft in
  Ok
    (Binary.Typedef.SimpleType
       (None, { final = true; ids = []; ct = Def_func_t ft }) )

let modul (modul : Grouped.t) (assigned : Assigned.t) : Binary.Module.t Result.t
    =
  Log.debug (fun m -> m "rewriting    ...");
  let* global =
    let f_local g = rewrite_global assigned g in
    let f_imported (m, val_type) =
      let+ val_type = rewrite_val_type assigned val_type in
      (m, val_type)
    in
    array_map (Origin.monadic_map ~f_local ~f_imported) modul.global
  in
  let* table =
    let f_local g = rewrite_table assigned g in
    let f_imported (l, (n, ht)) =
      let* l = rewrite_table_limits l in
      let+ ht = rewrite_heap_type assigned ht in
      (l, (n, ht))
    in
    array_map (Origin.monadic_map ~f_local ~f_imported) modul.table
  in
  let* mem =
    let f_local g = rewrite_memory g in
    let f_imported = rewrite_memory_limits in
    array_map (Origin.monadic_map ~f_local ~f_imported) modul.mem
  in
  let* elem = array_map (rewrite_elem assigned) modul.elem in
  let* data = array_map (rewrite_data assigned) modul.data in
  let* exports = rewrite_exports modul assigned in
  let* func =
    let f_imported bt =
      let+ _, rt = rewrite_block_type assigned bt in
      rt
    in
    let f_local = rewrite_func assigned in
    let runtime = Origin.monadic_map ~f_local ~f_imported in
    array_map runtime modul.func
  in
  let* tag =
    let f_imported bt =
      let+ _, rt = rewrite_block_type assigned bt in
      rt
    in
    let f_local = rewrite_tag assigned in
    let runtime = Origin.monadic_map ~f_local ~f_imported in
    array_map runtime modul.tag
  in
  let* types =
    array_map (rewrite_types assigned) (Assigned.get_types assigned)
  in
  let+ start =
    match modul.start with
    | None -> Ok None
    | Some id ->
      let+ id = Assigned.find_func assigned id in
      Some id
  in

  { Binary.Module.id = modul.id
  ; mem
  ; table
  ; types
  ; global
  ; elem
  ; data
  ; exports
  ; func
  ; tag
  ; start
  ; custom = []
  }
