open Crowbar
open Crowbar.Syntax
open Owi.Text
module S = Type_stack
module B = Basic

let expr_always_available block loop expr ~locals ~stack env =
  [ pair B.const_i32 (const [ S.Push (Num_type I32) ])
  ; pair B.const_i64 (const [ S.Push (Num_type I64) ])
  ; pair B.const_f32 (const [ S.Push (Num_type F32) ])
  ; pair B.const_f64 (const [ S.Push (Num_type F64) ])
  ; pair (const Nop) (const [ S.Nothing ])
  ; block expr ~locals ~stack env
  ; loop expr ~locals ~stack env
  ; B.unreachable
  ]
  @ B.global_i32 env @ B.global_i64 env @ B.global_f32 env @ B.global_f64 env
  @ B.local_i32 env @ B.local_i64 env @ B.local_f32 env @ B.local_f64 env
  @ B.data_drop env @ B.elem_drop env
  @ (if B.memory_exists env then [ B.memory_size env ] else [])
  @ B.table_size env

let expr_available_1_any = [ pair (const Drop) (const [ S.Pop ]) ]

let expr_available_1_i32 if_else expr ~locals ~stack env =
  let load_instr =
    [ pair (B.i32_load env) (const [ S.Pop; S.Push (Num_type I32) ])
    ; pair (B.i64_load env) (const [ S.Pop; S.Push (Num_type I64) ])
    ; pair (B.f32_load env) (const [ S.Pop; S.Push (Num_type F32) ])
    ; pair (B.f64_load env) (const [ S.Pop; S.Push (Num_type F64) ])
    ; pair (B.i32_load8 env) (const [ S.Nothing ])
    ; pair (B.i32_load16 env) (const [ S.Nothing ])
    ; pair (B.i64_load8 env) (const [ S.Pop; S.Push (Num_type I64) ])
    ; pair (B.i64_load16 env) (const [ S.Pop; S.Push (Num_type I64) ])
    ; pair (B.i64_load32 env) (const [ S.Pop; S.Push (Num_type I64) ])
    ]
  in
  [ pair B.iunop_32 (const [ S.Nothing ])
  ; pair B.itestop_32 (const [ S.Nothing ])
  ; pair B.i64_extend_i32 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.extend_32_i32 (const [ S.Nothing ])
  ; pair B.f32_convert_i32 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.f64_convert_i32 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.f32_reinterpret_i32 (const [ S.Pop; S.Push (Num_type F32) ])
  ; if_else expr ~locals ~stack env
  ]
  @ B.local_set_i32 env @ B.local_tee_i32 env @ B.global_set_i32 env
  @ (if B.memory_exists env then B.memory_grow env :: load_instr else [])
  @ B.expr_br_if env stack @ B.table_get env

let expr_available_2_i32 (env : Env.t) =
  let store_instr =
    [ pair (B.i32_store env) (const [ S.Pop; S.Pop ])
    ; pair (B.i32_store8 env) (const [ S.Pop; S.Pop ])
    ; pair (B.i32_store16 env) (const [ S.Pop; S.Pop ])
    ]
  in
  [ pair B.ibinop_32 (const [ S.Pop ]); pair B.irelop_32 (const [ S.Pop ]) ]
  @ if B.memory_exists env then store_instr else []

let expr_available_2_i64_i32 (env : Env.t) =
  if B.memory_exists env then
    [ pair (B.i64_store env) (const [ S.Pop; S.Pop ])
    ; pair (B.i64_store8 env) (const [ S.Pop; S.Pop ])
    ; pair (B.i64_store16 env) (const [ S.Pop; S.Pop ])
    ; pair (B.i64_store32 env) (const [ S.Pop; S.Pop ])
    ]
  else []

let expr_available_2_f32_i32 (env : Env.t) =
  if B.memory_exists env then
    [ pair (B.f32_store env) (const [ S.Pop; S.Pop ]) ]
  else []

let expr_available_2_f64_i32 (env : Env.t) =
  if B.memory_exists env then
    [ pair (B.f64_store env) (const [ S.Pop; S.Pop ]) ]
  else []

let expr_available_3_i32 env =
  let mem_expr =
    if B.memory_exists env then
      [ B.memory_copy env; B.memory_fill env ] @ B.memory_init env
    else []
  in
  let table_expr = B.table_init env @ B.table_copy env in
  mem_expr @ table_expr

let expr_available_1_i64 env =
  [ pair B.iunop_64 (const [ S.Nothing ])
  ; pair B.itestop_64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i32_wrap_i64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.extend_64_i64 (const [ S.Nothing ])
  ; pair B.f32_convert_i64 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.f64_convert_i64 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.f64_reinterpret_i64 (const [ S.Pop; S.Push (Num_type F64) ])
  ]
  @ B.local_set_i64 env @ B.local_tee_i64 env @ B.global_set_i64 env

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
  @ B.local_set_f32 env @ B.local_tee_f32 env @ B.global_set_f32 env

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
  @ B.local_set_f64 env @ B.local_tee_f64 env @ B.global_set_f64 env

let expr_available_2_f64 =
  [ pair B.fbinop_64 (const [ S.Pop ])
  ; pair B.frelop_64 (const [ S.Pop; S.Pop; S.Push (Num_type I32) ])
  ]

(* let expr_available_3_f64 = [] *)

let if_else expr ~locals ~stack env =
  match stack with
  | Num_type I32 :: stack -> begin
    let* rt = list B.val_type
    and+ pt = B.stack_prefix stack in
    let typ =
      Bt_raw (None, (List.rev_map (fun t -> (None, t)) pt, List.rev rt))
    in
    let id = Env.add_block env typ Env.Block in
    (* same behavior as block *)
    let old_fuel = env.Env.fuel in
    env.fuel <- old_fuel / 2;
    let* expr_then = expr ~block_type:typ ~stack:pt ~locals env
    and+ expr_else =
      env.fuel <- old_fuel / 2;
      expr ~block_type:typ ~stack:pt ~locals env
    in
    env.fuel <- old_fuel / 2;
    Env.remove_block env;
    let+ instr = const @@ If_else (Some id, Some typ, expr_then, expr_else)
    and+ pt_descr = const @@ (S.Pop :: List.map (fun _ -> S.Pop) pt)
    and+ rt_descr = const @@ List.rev_map (fun t -> S.Push t) rt in
    (instr, pt_descr @ rt_descr)
  end
  | _ -> assert false

let block expr ~locals ~stack env =
  let* rt = list B.val_type
  and+ pt = B.stack_prefix stack in
  let typ =
    Bt_raw (None, (List.rev_map (fun t -> (None, t)) pt, List.rev rt))
  in
  let id = Env.add_block env typ Env.Block in
  let* expr = expr ~block_type:typ ~stack:pt ~locals env in
  Env.remove_block env;
  let+ instr = const @@ Block (Some id, Some typ, expr)
  and+ pt_descr = const @@ List.map (fun _ -> S.Pop) pt
  and+ rt_descr = const @@ List.rev_map (fun t -> S.Push t) rt in
  (instr, pt_descr @ rt_descr)

let loop expr ~locals ~stack env : (instr * S.stack_op list) gen =
  let* rt = list B.val_type
  and+ pt = B.stack_prefix stack in
  let typ =
    Bt_raw (None, (List.rev_map (fun t -> (None, t)) pt, List.rev rt))
  in
  let id = Env.add_block env typ Env.Loop in
  let* expr = expr ~block_type:typ ~stack:pt ~locals env in
  Env.remove_block env;
  let+ instr = const @@ Loop (Some id, Some typ, expr)
  and+ pt_descr = const @@ List.map (fun _ -> S.Pop) pt
  and+ rt_descr = const @@ List.rev_map (fun t -> S.Push t) rt in
  (instr, pt_descr @ rt_descr)

let rec expr ~block_type ~stack ~locals env : expr Owi.Annotated.t gen =
  let _pt, rt =
    match block_type with
    | Bt_raw (_indice, (pt, rt)) -> (pt, rt)
    | _ -> assert false
  in
  Env.use_fuel env;
  if Env.has_no_fuel env then
    match (rt, stack) with
    | [], [] -> const (Owi.Annotated.dummy_deep [ Nop ])
    | rt, l ->
      (* TODO: if we have a matching prefix, keep it *)
      (* TODO: try to consume them instead of just dropping *)
      let+ drops = const (List.map (fun _typ -> Drop) l)
      and+ adds =
        List.fold_left
          (fun (acc : instr list gen) typ ->
            let+ acc
            and+ cst = B.const_of_val_type typ in
            cst :: acc )
          (const []) (List.rev rt)
      in
      Owi.Annotated.dummy_deep (drops @ adds)
  else
    let expr_available_with_current_stack =
      (* TODO: complete this *)
      match stack with
      | Num_type I32 :: Num_type I32 :: Num_type I32 :: _tl ->
        expr_available_1_any
        @ expr_available_1_i32 if_else expr ~stack ~locals env
        @ expr_available_2_i32 env @ expr_available_3_i32 env
      | Num_type I32 :: Num_type I32 :: _tl ->
        expr_available_1_any
        @ expr_available_1_i32 if_else expr ~stack ~locals env
        @ expr_available_2_i32 env
      | Num_type I32 :: Ref_type (_, Func_ht) :: Num_type I32 :: _tl ->
        B.table_fill env
      | Num_type I32 :: Ref_type (_, Func_ht) :: _tl -> B.table_grow env
      | Ref_type (_, Func_ht) :: Num_type I32 :: _tl -> B.table_set env
      | Num_type I64 :: Num_type I32 :: _tl -> expr_available_2_i64_i32 env
      | Num_type F32 :: Num_type I32 :: _tl -> expr_available_2_f32_i32 env
      | Num_type F64 :: Num_type I32 :: _tl -> expr_available_2_f64_i32 env
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
      expr_always_available block loop expr ~locals ~stack env
      @ expr_available_with_current_stack @ B.expr_call env stack
      (* TODO: Function calls can be improved: recursive calls are not processed *)
      @ B.expr_br env stack
    in
    let* i, ops = choose (expr_available env) in
    let+ next =
      let stack = S.apply_stack_ops stack ops in
      expr ~block_type ~stack ~locals env
    and+ i = const (Owi.Annotated.dummy i) in
    let res : expr = i :: Owi.Annotated.raw next in
    Owi.Annotated.dummy res

let data env : Module.Field.t gen =
  let+ mode = B.data_mode env
  (* TODO: add some real data ! *)
  and+ init = (*bytes*) const "tmp" in
  (* TODO: Issue #37 *)
  let id = Some (Env.add_data env) in
  Module.Field.Data { id; init; mode }

let memory env : Module.Field.t gen =
  (* TODO: fix time explosion https://github.com/OCamlPro/owi/pull/28#discussion_r1212835761 *)
  let sup = if true then 10 else 65537 in
  let* min = range sup in
  let+ max = option (range ~min (sup - min)) in
  let id = Some (Env.add_memory env) in
  Module.Field.Mem (id, { min; max })

let typ env : Module.Field.t gen =
  let+ typ = B.func_type in
  let id = Some (Env.add_type env typ) in
  Module.Field.Typedef (id, typ)

let elem env : Module.Field.t gen =
  let+ typ = B.ref_type
  and+ mode = B.elem_mode env in
  let id = Some (Env.add_elem env typ) in
  Module.Field.Elem { id; typ; init = []; mode; explicit_typ = false }

let table env : Module.Field.t gen =
  let+ typ = B.table_type in
  let id = Some (Env.add_table env typ) in
  Module.Field.Table { id; typ; init = None }

let global env : Module.Field.t gen =
  let* ((_mut, t) as typ) = B.global_type in
  let+ init = B.const_of_val_type t in
  let id = Some (Env.add_global env typ) in
  let init = [ init ] in
  let init = Owi.Annotated.dummy_deep init in
  Module.Field.Global { typ; init; id }

let func env : Module.Field.t gen =
  let* () = const () in
  Env.reset_locals env;
  Env.refill_fuel env;
  let* locals = list (B.param env)
  and+ type_f = B.block_type env in
  let (_name : string) = Env.add_block env type_f Env.Func in
  let+ body = expr ~block_type:type_f ~stack:[] ~locals env in
  Env.remove_block env;
  let id = Some (Env.add_func env type_f) in
  Module.Field.Func { type_f; locals; body; id }

let fields env : Module.Field.t list gen =
  let+ memories =
    (* No memory management in symbolic context.
       TODO: When implementation will be more advanced,
       reactivate and refine instruction by instruction (not_symbolic operator). *)
    match env.Env.conf with
    | Concrete -> list (memory env)
    | Symbolic -> const []
  and+ datas = list (data env)
  and+ types = list (typ env)
  and+ tables = list (table env)
  and+ elems = list (elem env)
  and+ globals = list (global env)
  and+ funcs = list (func env)
  and+ start_code =
    let* () = const () in
    Env.reset_locals env;
    Env.refill_fuel env;
    let type_f = Bt_raw (None, ([], [])) in
    let id = Some "start" in
    let+ body = expr ~block_type:type_f ~stack:[] ~locals:[] env in
    Module.Field.Func { type_f; locals = []; body; id }
  in
  let start = Module.Field.Start (Raw 0) in
  let funcs = start :: start_code :: funcs in
  memories @ datas @ types @ elems @ tables @ globals @ funcs

let modul conf =
  let id = Some "m" in
  let* env = const Env.empty in
  let+ fields = fields (env conf) in
  Module.{ id; fields }
