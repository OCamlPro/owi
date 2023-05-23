open Crowbarplus
open Owi.Types.Symbolic
open Syntax
module S = Type_stack
module B = Basic

let expr_always_available =
  (* TODO: complete this *)
  [ pair B.const_i32 (const [ S.Push (Num_type I32) ])
  ; pair B.const_i64 (const [ S.Push (Num_type I64) ])
  ; pair B.const_f32 (const [ S.Push (Num_type F32) ])
  ; pair B.const_f64 (const [ S.Push (Num_type F64) ])
  ; pair (const Nop) (const [ S.Nothing ])
    (* ; pair (const Unreachable) (const [ S.Nothing ]) TODO: check  *)
  ]

let expr_available_1_any = [ pair (const Drop) (const [ S.Pop ]) ]

let expr_available_1_i32 =
  [ pair B.iunop_32 (const [ S.Nothing ])
  ; pair B.itestop_32 (const [ S.Nothing ])
  ; pair B.extend_i32 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.extend_32_i32 (const [ S.Nothing ])
  ; pair B.f32_convert_i32 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.f64_convert_i32 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.f32_reinterpret_i32 (const [ S.Pop; S.Push (Num_type F32) ])
  ]

let expr_available_2_i32 =
  [ pair B.ibinop_32 (const [ S.Pop ]); pair B.irelop_32 (const [ S.Pop ]) ]

(* let expr_available_3_i32 = [] *)

let expr_available_1_i64 =
  [ pair B.iunop_64 (const [ S.Nothing ])
  ; pair B.itestop_64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i32_wrap_i64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.extend_64_i64 (const [ S.Nothing ])
  ; pair B.f32_convert_i64 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.f64_convert_i64 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.f64_reinterpret_i64 (const [ S.Pop; S.Push (Num_type F64) ])
  ]

let expr_available_2_i64 =
  [ pair B.ibinop_64 (const [ S.Pop ])
  ; pair B.irelop_64 (const [ S.Pop; S.Pop; S.Push (Num_type I32) ])
  ]

(* let expr_available_3_i64 = [] *)

let expr_available_1_f32 =
  [ pair B.funop_32 (const [ S.Nothing ])
  ; pair B.i32_trunc_f32 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_f32 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.i32_trunc_sat_f32 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_sat_f32 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.f64_promote_f32 (const [ S.Pop; S.Push (Num_type F64) ])
  ; pair B.i32_reinterpret_f32 (const [ S.Pop; S.Push (Num_type I32) ])
  ]

let expr_available_2_f32 =
  [ pair B.fbinop_32 (const [ S.Pop ])
  ; pair B.frelop_32 (const [ S.Pop; S.Pop; S.Push (Num_type I32) ])
  ]

(* let expr_available_3_f32 = [] *)

let expr_available_1_f64 =
  [ pair B.funop_64 (const [ S.Nothing ])
  ; pair B.i32_trunc_f64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_f64 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.i32_trunc_sat_f64 (const [ S.Pop; S.Push (Num_type I32) ])
  ; pair B.i64_trunc_sat_f64 (const [ S.Pop; S.Push (Num_type I64) ])
  ; pair B.f32_demote_f64 (const [ S.Pop; S.Push (Num_type F32) ])
  ; pair B.i64_reinterpret_f64 (const [ S.Pop; S.Push (Num_type I64) ])
  ]

let expr_available_2_f64 =
  [ pair B.fbinop_64 (const [ S.Pop ])
  ; pair B.frelop_64 (const [ S.Pop; S.Pop; S.Push (Num_type I32) ])
  ]

(* let expr_available_3_f64 = [] *)

let rec expr ~block_type ~stack ~locals =
  let _pt, rt =
    match block_type with
    | Arg.Bt_raw (_indice, (pt, rt)) -> (pt, rt)
    | _ -> assert false
  in
  Env.use_fuel ();
  if Env.has_no_fuel () then
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
      | Num_type I32 :: Num_type I32 :: _tl ->
        expr_available_1_any @ expr_available_1_i32 @ expr_available_2_i32
      | Num_type I64 :: Num_type I64 :: _tl ->
        expr_available_1_any @ expr_available_1_i64 @ expr_available_2_i64
      | Num_type I32 :: _tl -> expr_available_1_any @ expr_available_1_i32
      | Num_type I64 :: _tl -> expr_available_1_any @ expr_available_1_i64
      | Num_type F32 :: Num_type F32 :: _tl ->
        expr_available_1_any @ expr_available_1_f32 @ expr_available_2_f32
      | Num_type F64 :: Num_type F64 :: _tl ->
        expr_available_1_any @ expr_available_1_f64 @ expr_available_2_f64
      | Num_type F32 :: _tl -> expr_available_1_any @ expr_available_1_f32
      | Num_type F64 :: _tl -> expr_available_1_any @ expr_available_1_f64
      | _ -> []
    in
    let expr_available =
      expr_always_available @ expr_available_with_current_stack
    in
    let* i, ops = choose expr_available in
    let stack = S.apply_stack_ops stack ops in
    let next = expr ~block_type ~stack ~locals in
    let i = const i in
    list_cons i next


(* | If_else of string option * block_type option * expr * expr *)

let if_else =
  let* typ = B.block_type in
  let id = None in
  let+ expr_then = [ expr ~block_type:typ ~stack:[] ~locals:[] ] in
  let+ expr_else = [ expr ~block_type:typ ~stack:[] ~locals:[] ] in
  If_else (id, Some typ, expr_then, expr_else)

let global =
  let* ((_mut, t) as typ) = B.global_type in
  let+ init = B.const_of_val_type t in
  let id = Some (Env.add_global typ) in
  let init = [ init ] in
  MGlobal { typ; init; id }

let local = B.param

let func =
  let* locals = list local in
  let* type_f = B.block_type in
  let id = Some (Env.add_func type_f) in
  let+ body = expr ~block_type:type_f ~stack:[] ~locals in
  Env.reset_locals ();
  Env.refill_fuel ();
  MFunc { type_f; locals; body; id }

let fields =
  let globals = list global in
  let start =
    let type_f = Arg.Bt_raw (None, ([], [])) in
    let id = Some "start" in
    let+ body = expr ~block_type:type_f ~stack:[] ~locals:[] in
    MFunc { type_f; locals = []; body; id }
  in
  let funcs = list_cons start (list func) in
  list_append globals funcs

let modul =
  let start = MStart (Raw 0) in
  let id = Some "m" in
  let+ fields in
  { id; fields = start :: fields }
