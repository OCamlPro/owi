open Types
open Types.Simplified
open Value

let rec optimize_expr expr =
  match expr with
  | ((I32_const _ | I64_const _) as x)
    :: ((I32_const _ | I64_const _) as y)
    :: (I_binop (nn, op) as i_binop)
    :: tl -> begin
    try
      let result =
        Interpret.exec_ibinop [ Value.of_instr y; Value.of_instr x ] nn op
      in
      begin
        match result with
        | [ ((I32 _ | I64 _) as result) ] ->
          optimize_expr (Value.to_instr result :: tl)
        | _ -> assert false
      end
    with Trap _ -> x :: optimize_expr (y :: i_binop :: tl)
  end
  | ((F32_const _ | F64_const _) as x)
    :: ((F32_const _ | F64_const _) as y)
    :: F_binop (nn, op)
    :: tl ->
    let result =
      Interpret.exec_fbinop [ Value.of_instr y; Value.of_instr x ] nn op
    in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x) :: I_unop (nn, op) :: tl ->
    let result = Interpret.exec_iunop [ Value.of_instr x ] nn op in
    begin
      match result with
      | [ ((I32 _ | I64 _) as result) ] ->
        optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | ((F32_const _ | F64_const _) as x) :: F_unop (nn, op) :: tl ->
    let result = Interpret.exec_funop [ Value.of_instr x ] nn op in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x) :: I_testop (nn, op) :: tl ->
    let result = Interpret.exec_itestop [ Value.of_instr x ] nn op in
    begin
      match result with
      | [ (I32 _ as result) ] -> optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x)
    :: ((I32_const _ | I64_const _) as y)
    :: I_relop (nn, op)
    :: tl ->
    let result =
      Interpret.exec_irelop [ Value.of_instr y; Value.of_instr x ] nn op
    in
    begin
      match result with
      | [ (I32 _ as result) ] -> optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | ((F32_const _ | F64_const _) as x)
    :: ((F32_const _ | F64_const _) as y)
    :: F_relop (nn, op)
    :: tl ->
    let result =
      Interpret.exec_frelop [ Value.of_instr y; Value.of_instr x ] nn op
    in
    begin
      match result with
      | [ (I32 _ as result) ] -> optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | I32_const c :: I_extend8_s S32 :: tl ->
    optimize_expr (I32_const (Int32.extend_s 8 c) :: tl)
  | I64_const c :: I_extend8_s S64 :: tl ->
    optimize_expr (I64_const (Int64.extend_s 8 c) :: tl)
  | I32_const c :: I_extend16_s S32 :: tl ->
    optimize_expr (I32_const (Int32.extend_s 16 c) :: tl)
  | I64_const c :: I_extend16_s S64 :: tl ->
    optimize_expr (I64_const (Int64.extend_s 16 c) :: tl)
  | I64_const c :: I64_extend32_s :: tl ->
    optimize_expr (I64_const (Int64.extend_s 32 c) :: tl)
  | I64_const c :: I32_wrap_i64 :: tl ->
    optimize_expr (I32_const (Convert.Int32.wrap_i64 c) :: tl)
  | I32_const c :: I64_extend_i32 s :: tl -> begin
    match s with
    | S -> optimize_expr (I64_const (Convert.Int64.extend_i32_s c) :: tl)
    | U -> optimize_expr (I64_const (Convert.Int64.extend_i32_u c) :: tl)
  end
  | F64_const c :: F32_demote_f64 :: tl ->
    optimize_expr (F32_const (Convert.Float32.demote_f64 c) :: tl)
  | F32_const c :: F64_promote_f32 :: tl ->
    optimize_expr (F64_const (Convert.Float64.promote_f32 c) :: tl)
  | ((F32_const _ | F64_const _) as x)
    :: ((F32_const _ | F64_const _) as y)
    :: (I_trunc_f (nn, nn', sx) as i_truncf)
    :: tl -> begin
    try
      let result =
        Interpret.exec_itruncf [ Value.of_instr y; Value.of_instr x ] nn nn' sx
      in
      begin
        match result with
        | [ ((I32 _ | I64 _) as result) ] ->
          optimize_expr (Value.to_instr result :: tl)
        | _ -> assert false
      end
    with Trap _ -> x :: optimize_expr (y :: i_truncf :: tl)
  end
  | ((F32_const _ | F64_const _) as x)
    :: ((F32_const _ | F64_const _) as y)
    :: (I_trunc_sat_f (nn, nn', sx) as i_truncsatf)
    :: tl -> begin
    try
      let result =
        Interpret.exec_itruncsatf
          [ Value.of_instr y; Value.of_instr x ]
          nn nn' sx
      in
      begin
        match result with
        | [ ((I32 _ | I64 _) as result) ] ->
          optimize_expr (Value.to_instr result :: tl)
        | _ -> assert false
      end
    with Trap _ -> x :: optimize_expr (y :: i_truncsatf :: tl)
  end
  | ((I32_const _ | I64_const _) as x) :: F_convert_i (nn, nn', sx) :: tl ->
    let result = Interpret.exec_fconverti [ Value.of_instr x ] nn nn' sx in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | ((F32_const _ | F64_const _) as x) :: I_reinterpret_f (nn, nn') :: tl ->
    let result = Interpret.exec_ireinterpretf [ Value.of_instr x ] nn nn' in
    begin
      match result with
      | [ ((I32 _ | I64 _) as result) ] ->
        optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x) :: F_reinterpret_i (nn, nn') :: tl ->
    let result = Interpret.exec_freinterpreti [ Value.of_instr x ] nn nn' in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        optimize_expr (Value.to_instr result :: tl)
      | _ -> assert false
    end
  | (I32_const _ | I64_const _ | F32_const _ | F64_const _) :: Drop :: tl ->
    optimize_expr tl
  | Local_set x :: Local_get y :: tl when x = y ->
    optimize_expr (Local_tee x :: tl)
  | (Br _ as br) :: _tl -> [ br ]
  | I32_const c :: Br_if l :: tl -> begin
    match c with 0l -> optimize_expr tl | _ -> [ Br l ]
  end
  | Return :: _tl -> [ Return ]
  | (Return_call _ as rc) :: _tl -> [ rc ]
  | (Return_call_indirect _ as rci) :: _tl -> [ rci ]
  | Ref_null _ :: Ref_is_null :: tl -> optimize_expr (I32_const 1l :: tl)
  | Nop :: tl -> optimize_expr tl
  | ((I32_const _ | I64_const _ | F32_const _ | F64_const _) as v1)
    :: ((I32_const _ | I64_const _ | F32_const _ | F64_const _) as v2)
    :: I32_const c
    :: Select _ :: tl -> begin
    match c with 0l -> optimize_expr (v2 :: tl) | _ -> optimize_expr (v1 :: tl)
  end
  | Block (n, bt, e) :: tl -> begin
    match optimize_expr e with
    | [] -> optimize_expr tl
    | oe -> Block (n, bt, oe) :: optimize_expr tl
  end
  | Loop (n, bt, e) :: tl -> begin
    match optimize_expr e with
    | [] -> optimize_expr tl
    | oe -> Loop (n, bt, oe) :: optimize_expr tl
  end
  | I32_const 0l :: If_else (n, bt, _e1, e2) :: tl ->
    optimize_expr (Block (n, bt, e2) :: tl)
  | I32_const _ :: If_else (n, bt, e1, _e2) :: tl ->
    optimize_expr (Block (n, bt, e1) :: tl)
  | If_else (n, bt, e1, e2) :: tl -> begin
    match (optimize_expr e1, optimize_expr e2) with
    | [], [] -> Drop :: optimize_expr tl
    | oe1, oe2 -> If_else (n, bt, oe1, oe2) :: optimize_expr tl
  end
  | hd :: tl -> hd :: optimize_expr tl
  | [] -> []

let locals_func body_expr =
  let locals_hashtbl = Hashtbl.create 16 in
  let rec aux_instr instr =
    match instr with
    | Local_get ind | Local_set ind | Local_tee ind ->
      Hashtbl.add locals_hashtbl ind ()
    | Block (_, _, e) -> aux_expr e
    | Loop (_, _, e) -> aux_expr e
    | If_else (_, _, e1, e2) ->
      aux_expr e1;
      aux_expr e2
    | I64_extend32_s | I32_wrap_i64 | F32_demote_f64 | F64_promote_f32
    | Ref_is_null | Ref_as_non_null | Ref_eq | Drop | Memory_size | Memory_grow
    | Memory_fill | Memory_copy | Nop | Unreachable | Return | Array_len
    | I31_get_u | I31_get_s | I31_new | Extern_externalize | Extern_internalize
    | I32_const _ | I64_const _ | F32_const _ | F64_const _
    | I_unop (_, _)
    | F_unop (_, _)
    | I_binop (_, _)
    | F_binop (_, _)
    | I_testop (_, _)
    | I_relop (_, _)
    | F_relop (_, _)
    | I_extend8_s _ | I_extend16_s _ | I64_extend_i32 _
    | I_trunc_f (_, _, _)
    | I_trunc_sat_f (_, _, _)
    | F_convert_i (_, _, _)
    | I_reinterpret_f (_, _)
    | F_reinterpret_i (_, _)
    | Ref_null _ | Ref_func _
    | Ref_cast (_, _)
    | Ref_test (_, _)
    | Select _ | Global_get _ | Global_set _ | Table_get _ | Table_set _
    | Table_size _ | Table_grow _ | Table_fill _
    | Table_copy (_, _)
    | Table_init (_, _)
    | Elem_drop _
    | I_load (_, _)
    | F_load (_, _)
    | I_store (_, _)
    | F_store (_, _)
    | I_load8 (_, _, _)
    | I_load16 (_, _, _)
    | I64_load32 (_, _)
    | I_store8 (_, _)
    | I_store16 (_, _)
    | I64_store32 _ | Memory_init _ | Data_drop _ | Br _ | Br_if _
    | Br_table (_, _)
    | Br_on_cast (_, _, _)
    | Br_on_cast_fail (_, _, _)
    | Br_on_non_null _ | Br_on_null _ | Return_call _
    | Return_call_indirect (_, _)
    | Call _
    | Call_indirect (_, _)
    | Array_get _ | Array_get_u _ | Array_new_canon _
    | Array_new_canon_data (_, _)
    | Array_new_canon_default _
    | Array_new_canon_elem (_, _)
    | Array_new_canon_fixed (_, _)
    | Array_set _
    | Struct_get (_, _)
    | Struct_get_s (_, _)
    | Struct_new_canon _ | Struct_new_canon_default _
    | Struct_set (_, _) ->
      ()
  and aux_expr expr = List.iter aux_instr expr in
  aux_expr body_expr;
  locals_hashtbl

let locals_used_in_func locals nb_params body_expr =
  let loc_hashtbl = locals_func body_expr in
  let remove_local idx body_expr =
    let rec aux_instr instr =
      match instr with
      | Local_get ind when nb_params <= idx && idx < ind -> Local_get (ind - 1)
      | Local_set ind when nb_params <= idx && idx < ind -> Local_set (ind - 1)
      | Local_tee ind when nb_params <= idx && idx < ind -> Local_tee (ind - 1)
      | Block (m, t, e) -> Block (m, t, aux_expr e)
      | Loop (m, t, e) -> Loop (m, t, aux_expr e)
      | If_else (m, t, e1, e2) -> If_else (m, t, aux_expr e1, aux_expr e2)
      | I64_extend32_s | I32_wrap_i64 | F32_demote_f64 | F64_promote_f32
      | Ref_is_null | Ref_as_non_null | Ref_eq | Drop | Memory_size
      | Memory_grow | Memory_fill | Memory_copy | Nop | Unreachable | Return
      | Array_len | I31_get_u | I31_get_s | I31_new | Extern_externalize
      | Extern_internalize | I32_const _ | I64_const _ | F32_const _
      | F64_const _
      | I_unop (_, _)
      | F_unop (_, _)
      | I_binop (_, _)
      | F_binop (_, _)
      | I_testop (_, _)
      | I_relop (_, _)
      | F_relop (_, _)
      | I_extend8_s _ | I_extend16_s _ | I64_extend_i32 _
      | I_trunc_f (_, _, _)
      | I_trunc_sat_f (_, _, _)
      | F_convert_i (_, _, _)
      | I_reinterpret_f (_, _)
      | F_reinterpret_i (_, _)
      | Ref_null _ | Ref_func _
      | Ref_cast (_, _)
      | Ref_test (_, _)
      | Select _ | Local_get _ | Local_set _ | Local_tee _ | Global_get _
      | Global_set _ | Table_get _ | Table_set _ | Table_size _ | Table_grow _
      | Table_fill _
      | Table_copy (_, _)
      | Table_init (_, _)
      | Elem_drop _
      | I_load (_, _)
      | F_load (_, _)
      | I_store (_, _)
      | F_store (_, _)
      | I_load8 (_, _, _)
      | I_load16 (_, _, _)
      | I64_load32 (_, _)
      | I_store8 (_, _)
      | I_store16 (_, _)
      | I64_store32 _ | Memory_init _ | Data_drop _ | Br _ | Br_if _
      | Br_table (_, _)
      | Br_on_cast (_, _, _)
      | Br_on_cast_fail (_, _, _)
      | Br_on_non_null _ | Br_on_null _ | Return_call _
      | Return_call_indirect (_, _)
      | Call _
      | Call_indirect (_, _)
      | Array_get _ | Array_get_u _ | Array_new_canon _
      | Array_new_canon_data (_, _)
      | Array_new_canon_default _
      | Array_new_canon_elem (_, _)
      | Array_new_canon_fixed (_, _)
      | Array_set _
      | Struct_get (_, _)
      | Struct_get_s (_, _)
      | Struct_new_canon _ | Struct_new_canon_default _
      | Struct_set (_, _) ->
        instr
    and aux_expr expr = List.map aux_instr expr in
    aux_expr body_expr
  in
  let loop (idx, param_l, body_expr) param : int * param list * expr =
    match Hashtbl.find_opt loc_hashtbl idx with
    | None ->
      ( idx + 1
      , (if nb_params <= idx then param_l else List.append param_l [ param ])
      , remove_local idx body_expr )
    | Some _ -> (idx + 1, List.append param_l [ param ], body_expr)
  in
  let _, l, b = List.fold_left loop (0, [], body_expr) locals in
  (l, b)

let optimize_func func =
  let { type_f; locals; body; id } = func in
  let pt, _ = type_f in
  let nb_params = List.length pt in
  let locals, body = locals_used_in_func locals nb_params body in
  let body = optimize_expr body in
  { type_f; locals; body; id }

let optimize_runtime_func f =
  let { value; index } = f in
  match value with
  | Imported _ -> f
  | Local f ->
    let value = Local (optimize_func f) in
    { value; index }

let optimize_funcs funs = Named.map optimize_runtime_func funs

let modul m =
  Log.debug "optimizing   ...@\n";
  let func = optimize_funcs m.func in
  { m with func }
