open Crowbarplus
open Owi.Types
open Owi.Types.Symbolic
open Syntax

module S = Type_stack

let num_type = choose [ const I32; const I64; const F32; const F64 ]

let sx = choose [ const U; const S ]

let val_type =
  let+ num_type in
  Num_type num_type

let mut = choose [ const Const; const Var ]

let div =
  let+ sx in
  (Div sx : ibinop)

let rem =
  let+ sx in
  (Rem sx : ibinop)

let shr =
  let+ sx in
  (Shr sx : ibinop)

let ibinop =
  choose
    [ const (Add : ibinop)
    ; const (Sub : ibinop)
    ; const (Mul : ibinop)
    ; div
    ; rem
    ; const (And : ibinop)
    ; const (Or : ibinop)
    ; const (Xor : ibinop)
    ; const (Shl : ibinop)
    ; shr
    ; const (Rotl : ibinop)
    ; const (Rotr : ibinop)
    ]

let iunop = choose [ const Clz; const Ctz; const Popcnt ]

let itestop = const Eqz

let ilt =
  let+ sx in
  (Lt sx : irelop)

let igt =
  let+ sx in
  (Gt sx : irelop)

let ile =
  let+ sx in
  (Le sx : irelop)

let ige =
  let+ sx in
  (Ge sx : irelop)

let irelop =
  choose [ const (Eq : irelop); const (Ne : irelop); ilt; igt; ile; ige ]

let const_i32 =
  let+ int32 in
  I32_const int32

let const_i64 =
  let+ int64 in
  I64_const int64

let ibinop_32 =
  let+ ibinop in
  I_binop (S32, ibinop)

let ibinop_64 =
  let+ ibinop in
  I_binop (S64, ibinop)

let iunop_32 =
  let+ iunop in
  I_unop (S32, iunop)

let iunop_64 =
  let+ iunop in
  I_unop (S64, iunop)

let itestop_32 =
  let+ itestop in
  I_testop (S32, itestop)

let itestop_64 =
  let+ itestop in
  I_testop (S64, itestop)

let irelop_32 =
  let+ irelop in
  I_relop (S32, irelop)

let irelop_64 =
  let+ irelop in
  I_relop (S64, irelop)

let i32_wrap_i64 = const I32_wrap_i64

let i64_extend_i32 =
  let+ sx in
  I64_extend_i32 sx

let extend_32_i32 = choose [ const (I_extend8_s S32); const (I_extend16_s S32) ]

let extend_64_i64 =
  choose
    [ const (I_extend8_s S64); const (I_extend16_s S64); const I64_extend32_s ]

let funop =
  choose
    [ const Abs
    ; const Neg
    ; const Sqrt
    ; const Ceil
    ; const Floor
    ; const Trunc
    ; const Nearest
    ]

let fbinop =
  choose
    [ const Add
    ; const Sub
    ; const Mul
    ; const Div
    ; const Min
    ; const Max
    ; const Copysign
    ]

let frelop =
  choose [ const Eq; const Ne; const Lt; const Gt; const Le; const Ge ]

let fbinop_32 =
  let+ fbinop in
  F_binop (S32, fbinop)

let fbinop_64 =
  let+ fbinop in
  F_binop (S64, fbinop)

let funop_32 =
  let+ funop in
  F_unop (S32, funop)

let funop_64 =
  let+ funop in
  F_unop (S64, funop)

let frelop_32 =
  let+ frelop in
  F_relop (S32, frelop)

let frelop_64 =
  let+ frelop in
  F_relop (S64, frelop)

let const_f32 =
  let+ float in
  F32_const (Owi.Float32.of_float float)

let const_f64 =
  let+ float in
  F64_const (Owi.Float64.of_float float)

let f32_convert_i32 =
  let+ sx in
  F_convert_i (S32, S32, sx)

let f32_convert_i64 =
  let+ sx in
  F_convert_i (S32, S64, sx)

let f64_convert_i32 =
  let+ sx in
  F_convert_i (S64, S32, sx)

let f64_convert_i64 =
  let+ sx in
  F_convert_i (S64, S64, sx)

let i32_trunc_f32 =
  let+ sx in
  I_trunc_f (S32, S32, sx)

let i32_trunc_f64 =
  let+ sx in
  I_trunc_f (S32, S64, sx)

let i64_trunc_f32 =
  let+ sx in
  I_trunc_f (S64, S32, sx)

let i64_trunc_f64 =
  let+ sx in
  I_trunc_f (S64, S64, sx)

let i32_trunc_sat_f32 =
  let+ sx in
  I_trunc_sat_f (S32, S32, sx)

let i32_trunc_sat_f64 =
  let+ sx in
  I_trunc_sat_f (S32, S64, sx)

let i64_trunc_sat_f32 =
  let+ sx in
  I_trunc_sat_f (S64, S32, sx)

let i64_trunc_sat_f64 =
  let+ sx in
  I_trunc_sat_f (S64, S64, sx)

let f32_demote_f64 = const F32_demote_f64

let f64_promote_f32 = const F64_promote_f32

let i32_reinterpret_f32 = const (I_reinterpret_f (S32, S32))

let i64_reinterpret_f64 = const (I_reinterpret_f (S64, S64))

let f32_reinterpret_i32 = const (F_reinterpret_i (S32, S32))

let f64_reinterpret_i64 = const (F_reinterpret_i (S64, S64))

let global ntyp env =
  let globals = Env.get_globals ntyp env ~only_mut:false in
  List.map
    ( fun (name, (_, _)) ->
      pair (const (Global_get (Symbolic name))) (const [ S.Push (Num_type ntyp) ]) )
    globals

let global_i32 = global I32

let global_i64 = global I64

let global_f32 = global F32

let global_f64 = global F64

let global_set ntyp env =
  let globals = Env.get_globals ntyp env ~only_mut:true in
  List.map
    ( fun (name, (_, _)) ->
      pair (const (Global_set (Symbolic name))) (const [ S.Pop ]) )
    globals

let global_set_i32 = global_set I32

let global_set_i64 = global_set I64

let global_set_f32 = global_set F32

let global_set_f64 = global_set F64

let local ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    ( fun (name, _) ->
      pair (const (Local_get (Symbolic name))) (const [ S.Push (Num_type ntyp) ]) )
    locals

let local_i32 = local I32

let local_i64 = local I64

let local_f32 = local F32

let local_f64 = local F64

let local_set ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    ( fun (name, _) -> 
      pair (const (Local_set (Symbolic name))) (const [ S.Pop ]) )
    locals

let local_set_i32 = local_set I32

let local_set_i64 = local_set I64

let local_set_f32 = local_set F32

let local_set_f64 = local_set F64

let local_tee ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    ( fun (name, _) ->
      pair (const (Local_tee (Symbolic name))) (const [ S.Nothing ]) )
    locals

let local_tee_i32 = local_tee I32

let local_tee_i64 = local_tee I64

let local_tee_f32 = local_tee F32

let local_tee_f64 = local_tee F64

let const_of_num_type = function
  | I32 -> const_i32
  | I64 -> const_i64
  | F32 -> const_f32
  | F64 -> const_f64

let const_of_val_type = function
  | Num_type nt -> const_of_num_type nt
  | _ ->
    (* TODO: complete *)
    assert false

let global_type = pair mut val_type

let param env =
  let* typ = val_type in
  let name = Env.add_local env typ in
  const (Some name, typ)

let block_type env =
  let+ param_type = list (param env)
  and+ result_type = list val_type in
  Arg.Bt_raw (None, (param_type, result_type))

let memory_size =
  pair (const Memory_size) (const [ S.Push (Num_type I32) ])

let memory_grow =
  pair (const Memory_grow) (const [ S.Nothing ])

let memory_copy =
  pair (const Memory_copy) (const [ S.Pop; S.Pop; S.Pop ])

let memory_fill =
  pair (const Memory_fill) (const [ S.Pop; S.Pop; S.Pop ])

let memory_init (env : Env.t) =
  List.map
    ( fun name ->
      pair (const (Memory_init (Symbolic name))) (const [ S.Pop; S.Pop; S.Pop ]) )
    env.datas

let memory_exists (env : Env.t) =
  Option.is_some env.memory

let memarg =
  let* offset = int in
  let+ align = int in
  { offset; align}

let i32_load =
  let+ memarg in
  I_load (S32, memarg)

let i64_load =
  let+ memarg in
  I_load (S64, memarg)

let f32_load =
  let+ memarg in
  F_load (S32, memarg)

let f64_load =
  let+ memarg in
  F_load (S64, memarg)

let i32_load8 =
  let* memarg in
  let+ sx in
  I_load8 (S32, sx, memarg)

let i32_load16 =
  let* memarg in
  let+ sx in
  I_load16 (S32, sx, memarg)

let i64_load8 =
  let* memarg in
  let+ sx in
  I_load8 (S64, sx, memarg)

let i64_load16 =
  let* memarg in
  let+ sx in
  I_load16 (S64, sx, memarg)

let i64_load32 =
  let* memarg in
  let+ sx in
  I64_load32 (sx, memarg)

let i32_store =
  let+ memarg in
  I_store (S32, memarg)

let i64_store =
  let+ memarg in
  I_store (S64, memarg)

let f32_store =
  let+ memarg in
  F_store (S32, memarg)

let f64_store =
  let+ memarg in
  F_store (S64, memarg)

let i32_store8 =
  let+ memarg in
  I_store8 (S32, memarg)

let i64_store8 =
  let+ memarg in
  I_store8 (S64, memarg)

let i32_store16 =
  let+ memarg in
  I_store16 (S32, memarg)

let i64_store16 =
  let+ memarg in
  I_store16 (S64, memarg)

let i64_store32 =
  let+ memarg in
  I64_store32 memarg

let data_active name =
  let+ inst = const_i32 in
  let exp = [ inst ] in
  Data_active (Some (Symbolic name), exp)

let data_mode (env : Env.t) =
  match env.memory with
  | Some name -> choose [ const Data_passive; data_active name ]
  | None -> const Data_passive

let data_drop (env : Env.t) =
  List.map
    ( fun name ->
      pair (const (Data_drop (Symbolic name))) (const [ S.Nothing ]) )
    env.datas
