open Crowbarplus
open Owi.Types.Symbolic
open Syntax
module S = Type_stack
module B = Basic

let expr_always_available env =
  [ pair B.const_i32 (const [ S.Push (Num_type I32) ])
  ; pair B.const_i64 (const [ S.Push (Num_type I64) ])
  ; pair B.const_f32 (const [ S.Push (Num_type F32) ])
  ; pair B.const_f64 (const [ S.Push (Num_type F64) ])
  ; pair B.memory_size (const [ S.Push (Num_type I32) ])
  ; pair (const Nop) (const [ S.Nothing ])
    (* ; pair (const Unreachable) (const [ S.Nothing ]) TODO: check  *)
  ]
  @ (B.global_i32 env)
  @ (B.global_i64 env)
  @ (B.global_f32 env)
  @ (B.global_f64 env)
  @ (B.local_i32 env)
  @ (B.local_i64 env)
  @ (B.local_f32 env)
  @ (B.local_f64 env)
  @ (B.data_drop env)

let expr_available_1_any = [ pair (const Drop) (const [ S.Pop ]) ]

let expr_available_1_i32 if_else expr ~locals ~stack env =
  [ pair B.iunop_32 (const [ S.Nothing ])
  ; pair B.itestop_32 (const [ S.Nothing ])
  ; pair B.i64_extend_i32 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.extend_32_i32 (const [ S.Nothing ])
  ; pair B.f32_convert_i32 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.f64_convert_i32 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.f32_reinterpret_i32 (const [ S.Pop; S.Push (Num_type F32) ])
  ; if_else expr ~locals ~stack env
  ; pair B.memory_grow (const [ S.Nothing ])
  ; pair B.i32_load (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_load (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.f32_load (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.f64_load (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.i32_load8 (const [ S.Nothing ])
  ; pair B.i32_load16 (const [ S.Nothing ])
  ; pair B.i64_load8 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.i64_load16 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.i64_load32 (const [ S.Pop; S.Push (Num_type I64) ])
  ]
  @ (B.local_set_i32 env) @ (B.local_tee_i32 env)
  @ (B.global_set_i32 env)

let expr_available_2_i32 =
  [ pair B.ibinop_32 (const [ S.Pop ])
  ; pair B.irelop_32 (const [ S.Pop ])
  ; pair B.i32_store (const [ S.Pop; S.Pop ])
  ; pair B.i32_store8 (const [ S.Pop; S.Pop ])
  ; pair B.i32_store16 (const [ S.Pop; S.Pop ])
  ]

let expr_available_2_i64_i32 =
  [ pair B.i64_store (const [ S.Pop; S.Pop ])
  ; pair B.i64_store8 (const [ S.Pop; S.Pop ])
  ; pair B.i64_store16 (const [ S.Pop; S.Pop ])
  ; pair B.i64_store32 (const [ S.Pop; S.Pop ])
  ]

let expr_available_3_i32 =
  [ (*pair B.memory_init (const [ S.Pop; S.Pop; S.Pop ])
  ;*) pair B.memory_copy (const [ S.Pop; S.Pop; S.Pop ])
  (* ; pair B.memory_fill (const [ S.Pop; S.Pop; S.Pop ]) *)
  ]

let expr_available_1_i64 env =
  [ pair B.iunop_64 (const [ S.Nothing ])
  ; pair B.itestop_64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i32_wrap_i64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.extend_64_i64 (const [ S.Nothing ])
  ; pair B.f32_convert_i64 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.f64_convert_i64 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.f64_reinterpret_i64 (const [ S.Pop; S.Push (Num_type F64) ])
  ]
  @ (B.local_set_i64 env) @ (B.local_tee_i64 env)
  @ (B.global_set_i64 env)

let expr_available_2_i64 =
  [ pair B.ibinop_64 (const [ S.Pop ])
  ; pair B.irelop_64 (const [ S.Pop; S.Pop; S.Push (Num_type I32) ])
  ]

(* let expr_available_3_i64 = [] *)

let expr_available_1_f32 env =
  [ pair B.funop_32 (const [ S.Nothing ])
  ; pair B.i32_trunc_f32 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_f32 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.i32_trunc_sat_f32 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_sat_f32 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.f64_promote_f32 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.i32_reinterpret_f32 (const [ S.Pop; S.Push (Num_type I32) ])
  ]
  @ (B.local_set_f32 env) @ (B.local_tee_f32 env)
  @ (B.global_set_f32 env)

let expr_available_2_f32 =
  [ pair B.fbinop_32 (const [ S.Pop ])
  ; pair B.frelop_32 (const [ S.Pop; S.Pop; S.Push (Num_type I32) ])
  ]

(* let expr_available_3_f32 = [] *)

let expr_available_1_f64 env =
  [ pair B.funop_64 (const [ S.Nothing ])
  ; pair B.i32_trunc_f64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_f64 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.i32_trunc_sat_f64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_sat_f64 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.f32_demote_f64 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.i64_reinterpret_f64 (const [ S.Pop; S.Push (Num_type I64) ])
  ]
  @ (B.local_set_f64 env) @ (B.local_tee_f64 env)
  @ (B.global_set_f64 env)

let expr_available_2_f64 =
  [ pair B.fbinop_64 (const [ S.Pop ])
  ; pair B.frelop_64 (const [ S.Pop; S.Pop; S.Push (Num_type I32) ])
  ]

(* let expr_available_3_f64 = [] *)

let if_else expr ~locals ~stack env =
  (* TODO: finish > bug typechecking + List.rev *)
  match stack with
  | Num_type I32 :: _stack -> begin
    let rt = [] in
    (* let* rt = list B.val_type in *)
    let pt = [] in
    (* let pt = List.rev stack in *)
    (* TODO: take only a prefix *)
    let typ1 = Arg.Bt_raw (None, (List.rev_map (fun t -> (None, t)) pt, rt)) in
    let typ2 = Arg.Bt_raw (None, (List.rev_map (fun t -> (None, t)) pt, rt)) in
    (* let typ1 = Arg.Bt_raw (None, (List.map (fun t -> (None, t)) pt, rt)) in
       let _typ2 = Arg.Bt_raw (None, (List.map (fun t -> (None, t)) pt, rt)) in *)
    (* let pt = List.rev pt in *)
    let* expr_then = expr ~block_type:typ1 ~stack:pt ~locals env in
    let* expr_else = expr ~block_type:typ2 ~stack:pt ~locals env in
    let instr = If_else (None, Some typ1, expr_then, expr_else) in
    let pt_descr = S.Pop :: List.map (fun _ -> S.Pop) pt in
    let rt_descr = List.map (fun t -> S.Push t) rt in
    pair (const instr) (const (pt_descr @ rt_descr))
  end
  | _ -> assert false

let rec expr ~block_type ~stack ~locals env =
  let _pt, rt =
    match block_type with
    | Arg.Bt_raw (_indice, (pt, rt)) -> (pt, rt)
    | _ -> assert false
  in
  Env.use_fuel env;
  if Env.has_no_fuel env then
    match (rt, stack) with
    | [], [] -> const [ Nop ]
    | rt, l ->
      (* TODO: if we have a matching prefix, keep it *)
      (* TODO: try to consume them instead of just dropping *)
      let drops = const (List.map (fun _typ -> Drop) l) in
      let adds =
        List.fold_left
          (fun (acc : instr list gen) typ ->
            list_cons (B.const_of_val_type typ) acc )
          (const []) rt
      in
      list_append drops adds
  else
    let expr_available_with_current_stack =
      (* TODO: complete this *)
      match stack with
      | Num_type I32 :: Num_type I32 :: Num_type I32 :: _tl ->
        expr_available_1_any
        @ expr_available_1_i32 if_else expr ~stack ~locals env
        @ expr_available_2_i32
        @ expr_available_3_i32
      | Num_type I32 :: Num_type I32 :: _tl ->
        expr_available_1_any
        @ expr_available_1_i32 if_else expr ~stack ~locals env
        @ expr_available_2_i32
      | Num_type I64 :: Num_type I32 :: _tl ->
        expr_available_2_i64_i32
      | Num_type F32 :: Num_type I32 :: _tl ->
        [ pair B.f32_store (const [ S.Pop; S.Pop ]) ]
      | Num_type F64 :: Num_type I32 :: _tl ->
        [ pair B.f64_store (const [ S.Pop; S.Pop ]) ]
      | Num_type I64 :: Num_type I64 :: _tl ->
        expr_available_1_any @ expr_available_1_i64 env @ expr_available_2_i64
      | Num_type I32 :: _tl ->
        expr_available_1_any
        @ expr_available_1_i32 if_else expr ~stack ~locals env
      | Num_type I64 :: _tl -> expr_available_1_any @ expr_available_1_i64 env
      | Num_type F32 :: Num_type F32 :: _tl ->
        expr_available_1_any @ expr_available_1_f32 env @ expr_available_2_f32
      | Num_type F64 :: Num_type F64 :: _tl ->
        expr_available_1_any @ expr_available_1_f64 env @ expr_available_2_f64
      | Num_type F32 :: _tl -> expr_available_1_any @ expr_available_1_f32 env
      | Num_type F64 :: _tl -> expr_available_1_any @ expr_available_1_f64 env
      | _ -> []
    in
    let expr_available env =
      expr_always_available env @ expr_available_with_current_stack
    in
    let* i, ops = choose (expr_available env) in
    let stack = S.apply_stack_ops stack ops in
    let next = expr ~block_type ~stack ~locals env in
    let i = const i in
    list_cons i next

let data env =
  let* init = bytes in
  let+ mode = B.data_mode env in
  let id = Some (Env.add_data env) in
  MData { id; init; mode}

let memory env =
  let+ id = const @@ Env.add_memory env in
  MMem (Some id, { min = 0; max = None})

let global env =
  let* ((_mut, t) as typ) = B.global_type in
  let+ init = B.const_of_val_type t in
  let id = Some (Env.add_global env typ) in
  let init = [ init ] in
  MGlobal { typ; init; id }

let local = B.param

let func env =
  let* locals = list (local env) in
  let* type_f = B.block_type env in
  let id = Some (Env.add_func env type_f) in
  let+ body = expr ~block_type:type_f ~stack:[] ~locals env in
  Env.reset_locals env;
  Env.refill_fuel env;
  MFunc { type_f; locals; body; id }

let fields env =
  let memories = map ([memory env]) (fun m -> [m]) in (* for now, just one memory *)
  let datas = list (data env) in
  let globals = list (global env) in
  let start_code =
    let type_f = Arg.Bt_raw (None, ([], [])) in
    let id = Some "start" in
    let+ body = expr ~block_type:type_f ~stack:[] ~locals:[] env in
    MFunc { type_f; locals = []; body; id }
  in
  let start = const @@ MStart (Raw 0) in
  let funcs = list_cons start (list_cons start_code (list (func env))) in
  list_append datas (list_append memories (list_append globals funcs))

let modul =
  let id = Some "m" in
  let* env = const Env.empty in
  let+ fields = fields (env ()) in
  { id; fields }
