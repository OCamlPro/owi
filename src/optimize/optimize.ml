(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

let rec optimize_expr expr : bool * binary instr list =
  match expr with
  | ((I32_const _ | I64_const _) as x)
    :: ((I32_const _ | I64_const _) as y)
    :: (I_binop (nn, op) as i_binop)
    :: tl -> begin
    let result =
      Interpret.Concrete.exec_ibinop [ V.of_instr y; V.of_instr x ] nn op
    in
    begin
      match result with
      | Ok [ ((I32 _ | I64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | Error _ ->
        let has_changed, e = optimize_expr (y :: i_binop :: tl) in
        (has_changed, x :: e)
      | _ -> assert false
    end
  end
  | ((F32_const _ | F64_const _) as x)
    :: ((F32_const _ | F64_const _) as y)
    :: F_binop (nn, op)
    :: tl ->
    let result =
      Interpret.Concrete.exec_fbinop [ V.of_instr y; V.of_instr x ] nn op
    in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x) :: I_unop (nn, op) :: tl ->
    let result = Interpret.Concrete.exec_iunop [ V.of_instr x ] nn op in
    begin
      match result with
      | [ ((I32 _ | I64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | ((F32_const _ | F64_const _) as x) :: F_unop (nn, op) :: tl ->
    let result = Interpret.Concrete.exec_funop [ V.of_instr x ] nn op in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x) :: I_testop (nn, op) :: tl ->
    let result = Interpret.Concrete.exec_itestop [ V.of_instr x ] nn op in
    begin
      match result with
      | [ (I32 _ as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x)
    :: ((I32_const _ | I64_const _) as y)
    :: I_relop (nn, op)
    :: tl ->
    let result =
      Interpret.Concrete.exec_irelop [ V.of_instr y; V.of_instr x ] nn op
    in
    begin
      match result with
      | [ (I32 _ as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | ((F32_const _ | F64_const _) as x)
    :: ((F32_const _ | F64_const _) as y)
    :: F_relop (nn, op)
    :: tl ->
    let result =
      Interpret.Concrete.exec_frelop [ V.of_instr y; V.of_instr x ] nn op
    in
    begin
      match result with
      | [ (I32 _ as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | I32_const c :: I_extend8_s S32 :: tl ->
    let _has_changed, e =
      optimize_expr (I32_const (Int32.extend_s 8 c) :: tl)
    in
    (true, e)
  | I64_const c :: I_extend8_s S64 :: tl ->
    let _has_changed, e =
      optimize_expr (I64_const (Int64.extend_s 8 c) :: tl)
    in
    (true, e)
  | I32_const c :: I_extend16_s S32 :: tl ->
    let _has_changed, e =
      optimize_expr (I32_const (Int32.extend_s 16 c) :: tl)
    in
    (true, e)
  | I64_const c :: I_extend16_s S64 :: tl ->
    let _has_changed, e =
      optimize_expr (I64_const (Int64.extend_s 16 c) :: tl)
    in
    (true, e)
  | I64_const c :: I64_extend32_s :: tl ->
    let _has_changed, e =
      optimize_expr (I64_const (Int64.extend_s 32 c) :: tl)
    in
    (true, e)
  | I64_const c :: I32_wrap_i64 :: tl ->
    let _has_changed, e =
      optimize_expr (I32_const (Convert.Int32.wrap_i64 c) :: tl)
    in
    (true, e)
  | I32_const c :: I64_extend_i32 s :: tl -> begin
    let _has_changed, e =
      match s with
      | S -> optimize_expr (I64_const (Convert.Int64.extend_i32_s c) :: tl)
      | U -> optimize_expr (I64_const (Convert.Int64.extend_i32_u c) :: tl)
    in
    (true, e)
  end
  | F64_const c :: F32_demote_f64 :: tl ->
    let _has_changed, e =
      optimize_expr (F32_const (Convert.Float32.demote_f64 c) :: tl)
    in
    (true, e)
  | F32_const c :: F64_promote_f32 :: tl ->
    let _has_changed, e =
      optimize_expr (F64_const (Convert.Float64.promote_f32 c) :: tl)
    in
    (true, e)
  | ((F32_const _ | F64_const _) as c)
    :: (I_trunc_f (nn, nn', sx) as i_truncf)
    :: tl -> begin
    let result = Interpret.Concrete.exec_itruncf [ V.of_instr c ] nn nn' sx in
    begin
      match result with
      | Ok [ ((I32 _ | I64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | Error _ ->
        let has_changed, e = optimize_expr (i_truncf :: tl) in
        (has_changed, c :: e)
      | Ok _ -> assert false
    end
  end
  | ((F32_const _ | F64_const _) as c) :: I_trunc_sat_f (nn, nn', sx) :: tl ->
    begin
    let result =
      Interpret.Concrete.exec_itruncsatf [ V.of_instr c ] nn nn' sx
    in
    begin
      match result with
      | [ ((I32 _ | I64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  end
  | ((I32_const _ | I64_const _) as x) :: F_convert_i (nn, nn', sx) :: tl ->
    let result = Interpret.Concrete.exec_fconverti [ V.of_instr x ] nn nn' sx in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | ((F32_const _ | F64_const _) as x) :: I_reinterpret_f (nn, nn') :: tl ->
    let result =
      Interpret.Concrete.exec_ireinterpretf [ V.of_instr x ] nn nn'
    in
    begin
      match result with
      | [ ((I32 _ | I64 _) as result) ] ->
        optimize_expr (V.to_instr result :: tl)
      | _ -> assert false
    end
  | ((I32_const _ | I64_const _) as x) :: F_reinterpret_i (nn, nn') :: tl ->
    let result =
      Interpret.Concrete.exec_freinterpreti [ V.of_instr x ] nn nn'
    in
    begin
      match result with
      | [ ((F32 _ | F64 _) as result) ] ->
        let _has_changed, e = optimize_expr (V.to_instr result :: tl) in
        (true, e)
      | _ -> assert false
    end
  | (I32_const _ | I64_const _ | F32_const _ | F64_const _) :: Drop :: tl ->
    let _has_changed, e = optimize_expr tl in
    (true, e)
  | Local_set (Raw x) :: Local_get (Raw y) :: tl when x = y ->
    let _has_changed, e = optimize_expr (Local_tee (Raw x) :: tl) in
    (true, e)
  | Local_get _ :: Drop :: tl ->
    let _has_changed, e = optimize_expr tl in
    (true, e)
  | Global_get _ :: Drop :: tl ->
    let _has_changed, e = optimize_expr tl in
    (true, e)
  | (Br _ as br) :: _ :: _ -> (true, [ br ])
  | I32_const c :: Br_if l :: tl -> begin
    match c with
    | 0l ->
      let _has_changed, e = optimize_expr tl in
      (true, e)
    | _ -> (true, [ Br l ])
  end
  | Return :: _ :: _ -> (true, [ Return ])
  | (Return_call _ as rc) :: _ :: _ -> (true, [ rc ])
  | (Return_call_indirect _ as rci) :: _ :: _ -> (true, [ rci ])
  | Ref_null _ :: Ref_is_null :: tl ->
    let _has_changed, e = optimize_expr (I32_const 1l :: tl) in
    (true, e)
  | Nop :: tl ->
    let _has_changed, e = optimize_expr tl in
    (true, e)
  | ((I32_const _ | I64_const _ | F32_const _ | F64_const _) as v1)
    :: ((I32_const _ | I64_const _ | F32_const _ | F64_const _) as v2)
    :: I32_const c
    :: Select _ :: tl -> begin
    let _has_changed, e =
      match c with
      | 0l -> optimize_expr (v2 :: tl)
      | _ -> optimize_expr (v1 :: tl)
    in
    (true, e)
  end
  | Block (n, bt, e) :: tl -> begin
    match optimize_expr e with
    | has_changed, [] ->
      let has_changed', e = optimize_expr tl in
      (has_changed || has_changed', e)
    | has_changed, oe ->
      let has_changed', e = optimize_expr tl in
      (has_changed || has_changed', Block (n, bt, oe) :: e)
  end
  | Loop (n, bt, e) :: tl -> begin
    match optimize_expr e with
    | has_changed, [] ->
      let has_changed', e = optimize_expr tl in
      (has_changed || has_changed', e)
    | has_changed, oe ->
      let has_changed', e = optimize_expr tl in
      (has_changed || has_changed', Loop (n, bt, oe) :: e)
  end
  | I32_const 0l :: If_else (n, bt, _e1, e2) :: tl ->
    let _has_changed, e = optimize_expr (Block (n, bt, e2) :: tl) in
    (true, e)
  | I32_const _ :: If_else (n, bt, e1, _e2) :: tl ->
    let _has_changed, e = optimize_expr (Block (n, bt, e1) :: tl) in
    (true, e)
  | If_else (n, bt, e1, e2) :: tl -> begin
    let has_changed1, e1 = optimize_expr e1 in
    let has_changed2, e2 = optimize_expr e2 in
    match (e1, e2) with
    | [], [] ->
      let _has_changed, e = optimize_expr tl in
      (true, Drop :: e)
    | oe1, oe2 ->
      let has_changed', e = optimize_expr tl in
      ( has_changed1 || has_changed2 || has_changed'
      , If_else (n, bt, oe1, oe2) :: e )
  end
  | hd :: tl ->
    let has_changed, e = optimize_expr tl in
    (has_changed, hd :: e)
  | [] -> (false, [])

let locals_func (body_expr : binary expr) =
  let locals_hashtbl = Hashtbl.create 16 in
  let rec aux_instr (instr : binary instr) =
    match instr with
    | Local_get ind | Local_set ind | Local_tee ind ->
      Hashtbl.replace locals_hashtbl ind ()
    | Block (_, _, e) -> aux_expr e
    | Loop (_, _, e) -> aux_expr e
    | If_else (_, _, e1, e2) ->
      aux_expr e1;
      aux_expr e2
    | I64_extend32_s | I32_wrap_i64 | F32_demote_f64 | F64_promote_f32
    | Ref_is_null | Drop | Memory_size | Memory_grow | Memory_fill | Memory_copy
    | Nop | Unreachable | Return | Extern_externalize | Extern_internalize
    | I32_const _ | I64_const _ | F32_const _ | F64_const _ | V128_const _
    | I_unop (_, _)
    | F_unop (_, _)
    | I_binop (_, _)
    | F_binop (_, _)
    | V_ibinop (_, _)
    | I_testop (_, _)
    | I_relop (_, _)
    | F_relop (_, _)
    | I_extend8_s _ | I_extend16_s _ | I64_extend_i32 _
    | I_trunc_f (_, _, _)
    | I_trunc_sat_f (_, _, _)
    | F_convert_i (_, _, _)
    | I_reinterpret_f (_, _)
    | F_reinterpret_i (_, _)
    | Ref_null _ | Ref_func _ | Select _ | Global_get _ | Global_set _
    | Table_get _ | Table_set _ | Table_size _ | Table_grow _ | Table_fill _
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
    | Return_call_indirect (_, _)
    | Call _
    | Call_indirect (_, _)
    | Call_ref _ | Return_call_ref _ | Return_call _ ->
      ()
  and aux_expr expr = List.iter aux_instr expr in
  aux_expr body_expr;
  locals_hashtbl

let remove_local map body =
  let new_x (Raw x : binary indice) =
    let x = match Hashtbl.find_opt map x with None -> x | Some x -> x in
    Raw x
  in
  let rec aux_instr (instr : binary instr) : binary instr =
    match instr with
    | Local_get ind -> Local_get (new_x ind)
    | Local_set ind -> Local_set (new_x ind)
    | Local_tee ind -> Local_tee (new_x ind)
    | Block (m, t, e) -> Block (m, t, aux_expr e)
    | Loop (m, t, e) -> Loop (m, t, aux_expr e)
    | If_else (m, t, e1, e2) -> If_else (m, t, aux_expr e1, aux_expr e2)
    | I64_extend32_s | I32_wrap_i64 | F32_demote_f64 | F64_promote_f32
    | Ref_is_null | Drop | Memory_size | Memory_grow | Memory_fill | Memory_copy
    | Nop | Unreachable | Return | Extern_externalize | Extern_internalize
    | I32_const _ | I64_const _ | F32_const _ | F64_const _ | V128_const _
    | I_unop (_, _)
    | F_unop (_, _)
    | I_binop (_, _)
    | F_binop (_, _)
    | V_ibinop (_, _)
    | I_testop (_, _)
    | I_relop (_, _)
    | F_relop (_, _)
    | I_extend8_s _ | I_extend16_s _ | I64_extend_i32 _
    | I_trunc_f (_, _, _)
    | I_trunc_sat_f (_, _, _)
    | F_convert_i (_, _, _)
    | I_reinterpret_f (_, _)
    | F_reinterpret_i (_, _)
    | Ref_null _ | Ref_func _ | Select _ | Global_get _ | Global_set _
    | Table_get _ | Table_set _ | Table_size _ | Table_grow _ | Table_fill _
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
    | Return_call _
    | Return_call_indirect (_, _)
    | Call _
    | Call_indirect (_, _)
    | Call_ref _ | Return_call_ref _ ->
      instr
  and aux_expr expr = List.map aux_instr expr in
  aux_expr body

let remove_unused_locals locals nb_args body =
  let unused_locals =
    let used_locals = locals_func body in
    let locals = List.mapi (fun i _x -> Raw (nb_args + i)) locals in
    List.filter (fun x -> not @@ Hashtbl.mem used_locals x) locals
  in
  let rename_map = Hashtbl.create 16 in
  List.iteri
    (fun j x ->
      let name, _ = x in
      let _x = Option.value name ~default:"anon" in
      let count = ref 0 in
      for i = 0 to j do
        if List.mem (Raw (nb_args + i)) unused_locals then incr count
      done;
      if not @@ List.mem (Raw (nb_args + j)) unused_locals then begin
        Hashtbl.replace rename_map (nb_args + j) (nb_args + j - !count)
      end )
    locals;
  let locals = List.mapi (fun i x -> (nb_args + i, x)) locals in
  let locals =
    List.filter_map
      (fun (i, x) -> if List.mem (Raw i) unused_locals then None else Some x)
      locals
  in
  let body = remove_local rename_map body in
  (locals, body)

let optimize_func (func : binary func) =
  let { type_f; locals; body; id } = func in
  let rec loop has_changed e =
    if not has_changed then
      (* TODO: it should be enough to return e directly, but doing one more call to optimize_expr seems to perform more optimizations... *)
      snd @@ optimize_expr e
    else
      let has_changed, e = optimize_expr e in
      loop has_changed e
  in
  let body = loop true body in
  let (Bt_raw ((None | Some _), (pt, _))) = type_f in
  let nb_args = List.length pt in
  let locals, body = remove_unused_locals locals nb_args body in
  { type_f; locals; body; id }

let optimize_runtime_func = function
  | Runtime.Imported _ as f -> f
  | Local f -> Runtime.Local (optimize_func f)

let modul m =
  Logs.info (fun m -> m "optimizing   ...");
  let func = Array.map optimize_runtime_func m.Binary.Module.func in
  { m with func }
