open Crowbar
open Crowbar.Syntax
open Owi.Text
module S = Type_stack

type num_size =
  | NS8
  | NS16
  | NS32
  | NS64

let num_type = choose [ const I32; const I64; const F32; const F64 ]

let nullable = choose [ const No_null; const Null ]

let heap_type : heap_type Crowbar.gen = const Func_ht
(* TODO: complete this - Extern_ht and others *)

let ref_type = pair nullable heap_type

let limits =
  let sup =
    if true then 10 else 100000
    (* TODO: fix max size ? *)
  in
  let* min = range sup in
  let+ max = option (range ~min (sup - min)) in
  { min; max }

let table_type = pair limits ref_type

let sx = choose [ const U; const S ]

let val_type =
  let+ num_type in
  Num_type num_type

let param = pair (const (None : string option)) val_type

let func_type = pair (list param) (list val_type)

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
  let int32 = Owi.Concrete_i32.of_int32 int32 in
  I32_const int32

let const_i64 =
  let+ int64 in
  let int64 = Owi.Concrete_i64.of_int64 int64 in
  I64_const int64

let const_v128 =
  let* a = int64 in
  let+ b = int64 in
  let v128 = Owi.Concrete_v128.of_i64x2 a b in
  V128_const v128

let ibinop_32 : instr gen =
  let+ ibinop in
  I_binop (S32, ibinop)

let ibinop_64 : instr gen =
  let+ ibinop in
  I_binop (S64, ibinop)

let iunop_32 : instr gen =
  let+ iunop in
  I_unop (S32, iunop)

let iunop_64 : instr gen =
  let+ iunop in
  I_unop (S64, iunop)

let itestop_32 : instr gen =
  let+ itestop in
  I_testop (S32, itestop)

let itestop_64 : instr gen =
  let+ itestop in
  I_testop (S64, itestop)

let irelop_32 : instr gen =
  let+ irelop in
  I_relop (S32, irelop)

let irelop_64 : instr gen =
  let+ irelop in
  I_relop (S64, irelop)

let i32_wrap_i64 : instr gen = const I32_wrap_i64

let i64_extend_i32 : instr gen =
  let+ sx in
  I64_extend_i32 sx

let extend_32_i32 : instr gen =
  choose [ const (I_extend8_s S32); const (I_extend16_s S32) ]

let extend_64_i64 : instr gen =
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

let fbinop_32 : instr gen =
  let+ fbinop in
  F_binop (S32, fbinop)

let fbinop_64 : instr gen =
  let+ fbinop in
  F_binop (S64, fbinop)

let funop_32 : instr gen =
  let+ funop in
  F_unop (S32, funop)

let funop_64 : instr gen =
  let+ funop in
  F_unop (S64, funop)

let frelop_32 : instr gen =
  let+ frelop in
  F_relop (S32, frelop)

let frelop_64 : instr gen =
  let+ frelop in
  F_relop (S64, frelop)

let const_f32 : instr gen =
  let+ float in
  F32_const (Owi.Concrete_f32.of_float float)

let const_f64 : instr gen =
  let+ float in
  F64_const (Owi.Concrete_f64.of_float float)

let f32_convert_i32 : instr gen =
  let+ sx in
  F_convert_i (S32, S32, sx)

let f32_convert_i64 : instr gen =
  let+ sx in
  F_convert_i (S32, S64, sx)

let f64_convert_i32 : instr gen =
  let+ sx in
  F_convert_i (S64, S32, sx)

let f64_convert_i64 : instr gen =
  let+ sx in
  F_convert_i (S64, S64, sx)

let i32_trunc_f32 : instr gen =
  let+ sx in
  I_trunc_f (S32, S32, sx)

let i32_trunc_f64 : instr gen =
  let+ sx in
  I_trunc_f (S32, S64, sx)

let i64_trunc_f32 : instr gen =
  let+ sx in
  I_trunc_f (S64, S32, sx)

let i64_trunc_f64 : instr gen =
  let+ sx in
  I_trunc_f (S64, S64, sx)

let i32_trunc_sat_f32 : instr gen =
  let+ sx in
  I_trunc_sat_f (S32, S32, sx)

let i32_trunc_sat_f64 : instr gen =
  let+ sx in
  I_trunc_sat_f (S32, S64, sx)

let i64_trunc_sat_f32 : instr gen =
  let+ sx in
  I_trunc_sat_f (S64, S32, sx)

let i64_trunc_sat_f64 : instr gen =
  let+ sx in
  I_trunc_sat_f (S64, S64, sx)

let f32_demote_f64 : instr gen = const F32_demote_f64

let f64_promote_f32 : instr gen = const F64_promote_f32

let i32_reinterpret_f32 : instr gen = const (I_reinterpret_f (S32, S32))

let i64_reinterpret_f64 : instr gen = const (I_reinterpret_f (S64, S64))

let f32_reinterpret_i32 : instr gen = const (F_reinterpret_i (S32, S32))

let f64_reinterpret_i64 : instr gen = const (F_reinterpret_i (S64, S64))

let global ntyp env =
  let globals = Env.get_globals ntyp env ~only_mut:false in
  List.map
    (fun (name, (_, _)) ->
      pair (const (Global_get (Text name))) (const [ S.Push (Num_type ntyp) ]) )
    globals

let global_i32 env = global I32 env

let global_i64 env = global I64 env

let global_f32 env = global F32 env

let global_f64 env = global F64 env

let global_set ntyp env =
  let globals = Env.get_globals ntyp env ~only_mut:true in
  List.map
    (fun (name, (_, _)) ->
      pair (const (Global_set (Text name))) (const [ S.Pop ]) )
    globals

let global_set_i32 env = global_set I32 env

let global_set_i64 env = global_set I64 env

let global_set_f32 env = global_set F32 env

let global_set_f64 env = global_set F64 env

let local ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    (fun (name, _) ->
      pair (const (Local_get (Text name))) (const [ S.Push (Num_type ntyp) ]) )
    locals

let local_i32 env = local I32 env

let local_i64 env = local I64 env

let local_f32 env = local F32 env

let local_f64 env = local F64 env

let local_set ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    (fun (name, _) -> pair (const (Local_set (Text name))) (const [ S.Pop ]))
    locals

let local_set_i32 env = local_set I32 env

let local_set_i64 env = local_set I64 env

let local_set_f32 env = local_set F32 env

let local_set_f64 env = local_set F64 env

let local_tee ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    (fun (name, _) -> pair (const (Local_tee (Text name))) (const [ S.Nothing ]))
    locals

let local_tee_i32 env = local_tee I32 env

let local_tee_i64 env = local_tee I64 env

let local_tee_f32 env = local_tee F32 env

let local_tee_f64 env = local_tee F64 env

let const_of_num_type = function
  | I32 -> const_i32
  | I64 -> const_i64
  | F32 -> const_f32
  | F64 -> const_f64
  | V128 -> const_v128

let const_of_val_type = function
  | Num_type nt -> const_of_num_type nt
  | _ ->
    (* TODO: complete *)
    assert false

let global_type = pair mut val_type

let elem_active (env : Env.t) =
  List.map
    (fun (name, _) ->
      let+ ind = const (Some (Owi.Text.Text name))
      and+ instr = const_i32 in
      Owi.Text.Elem.Mode.Active (ind, [ instr ] |> Owi.Annotated.dummy_deep) )
    env.tables

let elem_mode (env : Env.t) =
  choose (const Owi.Text.Elem.Mode.Passive :: elem_active env)
(* TODO: complete Elem_declarative - elem_active *)

let param env =
  let+ typ = val_type in
  let name = Env.add_local env typ in
  (Some name, typ)

let block_type env =
  let+ param_type = list (param env)
  and+ result_type = list val_type in
  Bt_raw (None, (param_type, result_type))

let memid (env : Env.t) =
  let memories = List.map const env.memories in
  let+ memory = choose memories in
  Text memory

let memory_size env : (instr * S.stack_op list) gen =
  pair
    (let+ id = memid env in
     Memory_size id )
    (const [ S.Push (Num_type I32) ])

let memory_grow env : (instr * S.stack_op list) gen =
  pair
    (let+ id = memid env in
     Memory_grow id )
    (const [ S.Nothing ])

let memory_copy env : (instr * S.stack_op list) gen =
  pair
    (let+ id1 = memid env
     and+ id2 = memid env in
     Memory_copy (id1, id2) )
    (const [ S.Pop; S.Pop; S.Pop ])

let memory_fill env : (instr * S.stack_op list) gen =
  pair
    (let+ id = memid env in
     Memory_fill id )
    (const [ S.Pop; S.Pop; S.Pop ])

let memory_init (env : Env.t) =
  List.map
    (fun name ->
      pair
        (let+ id = memid env in
         Memory_init (id, Text name) )
        (const [ S.Pop; S.Pop; S.Pop ]) )
    env.datas

let memory_exists (env : Env.t) = List.compare_length_with env.memories 0 > 0

let memarg nsize =
  let+ offset = int64
  and+ align =
    match nsize with
    | NS8 -> const 0
    | NS16 -> range 1
    | NS32 -> range 2
    | NS64 -> range 3
  in
  let offset = Owi.Concrete_i64.of_int64 offset in
  let offset =
    if
      Owi.Concrete_i64.lt offset Owi.Concrete_i64.zero
      |> Owi.Concrete_boolean.to_bool
    then Owi.Concrete_i64.sub Owi.Concrete_i64.zero offset
    else offset
  in
  let align = Owi.Concrete_i64.of_int align in
  { offset; align }

let i32_load env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  I_load (id, S32, memarg)

let i64_load env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  I_load (id, S64, memarg)

let f32_load env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  F_load (id, S32, memarg)

let f64_load env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  F_load (id, S64, memarg)

let i32_load8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env
  and+ sx in
  I_load8 (id, S32, sx, memarg)

let i32_load16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env
  and+ sx in
  I_load16 (id, S32, sx, memarg)

let i64_load8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env
  and+ sx in
  I_load8 (id, S64, sx, memarg)

let i64_load16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env
  and+ sx in
  I_load16 (id, S64, sx, memarg)

let i64_load32 env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env
  and+ sx in
  I64_load32 (id, sx, memarg)

let i32_store env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  I_store (id, S32, memarg)

let i64_store env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  I_store (id, S64, memarg)

let f32_store env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  F_store (id, S32, memarg)

let f64_store env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  F_store (id, S64, memarg)

let i32_store8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env in
  I_store8 (id, S32, memarg)

let i64_store8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env in
  I_store8 (id, S64, memarg)

let i32_store16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env in
  I_store16 (id, S32, memarg)

let i64_store16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env in
  I_store16 (id, S64, memarg)

let i64_store32 env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  I64_store32 (id, memarg)

let data_active name =
  let+ inst = const_i32 in
  let exp = [ inst ] |> Owi.Annotated.dummy_deep in
  Owi.Text.Data.Mode.Active (Some (Text name), exp)

let data_mode (env : Env.t) =
  choose
    ( const Owi.Text.Data.Mode.Passive
    :: List.map (fun id -> data_active id) env.memories )

let data_drop (env : Env.t) =
  List.map
    (fun name -> pair (const (Data_drop (Text name))) (const [ S.Nothing ]))
    env.datas

let elem_drop (env : Env.t) =
  List.map
    (fun (name, _) -> pair (const (Elem_drop (Text name))) (const [ S.Nothing ]))
    env.elems

let table_init (env : Env.t) =
  match (env.tables, env.elems) with
  | [], _ | _, [] -> []
  | tables, elems ->
    let tables = List.map const tables in
    let elems = List.map const elems in
    let instr =
      let* name_t, _ = choose tables
      and+ name_e, _ = choose elems in
      pair
        (const (Table_init (Text name_t, Text name_e)))
        (const [ S.Pop; S.Pop; S.Pop ])
    in
    [ instr ]

let table_copy (env : Env.t) =
  match env.tables with
  | [] -> []
  | tables ->
    let tables = List.map const tables in
    let instr =
      let* name_x, (_lim_x, rt_x) = choose tables
      and+ name_y, (_lim_y, rt_y) = choose tables in
      match (rt_x, rt_y) with
      | ((Null, ht1), (Null, ht2) | (No_null, ht1), (No_null, ht2))
        when heap_type_eq ht1 ht2 ->
        pair
          (const (Table_copy (Text name_x, Text name_y)))
          (const [ S.Pop; S.Pop; S.Pop ])
      | _ -> pair (const Nop) (const [ S.Nothing ])
      (* TODO: avoid if ... then ... else pair (const (Nop)) (const [ S.Nothing ])
         https://github.com/OCamlPro/owi/pull/28#discussion_r1275222846 *)
    in
    [ instr ]

let table_size (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair (const (Table_size (Text name))) (const [ S.Push (Num_type I32) ]) )
    env.tables

let table_grow (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair
        (const (Table_grow (Text name)))
        (const [ S.Pop; S.Pop; S.Push (Num_type I32) ]) )
    env.tables

let table_fill (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair (const (Table_fill (Text name))) (const [ S.Pop; S.Pop; S.Pop ]) )
    env.tables

let table_set (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair (const (Table_set (Text name))) (const [ S.Pop; S.Pop ]) )
    env.tables

let table_get (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair
        (const (Table_get (Text name)))
        (const [ S.Pop; S.Push (Ref_type (No_null, Func_ht)) ]) )
    env.tables

let block_kind = choose [ const Env.Block; const Env.Loop; const Env.Func ]

let expr_call (env : Env.t) (stack : val_type list) =
  let stack_pt = List.map (fun _ -> S.Pop) in
  let stack_rt = List.map (fun vt -> S.Push vt) in
  List.filter_map
    (fun (name, bt) ->
      match bt with
      | Bt_raw (_, (pt, rt))
        when S.is_stack_compatible_param stack (List.rev pt) ->
        Some
          (pair (const (Call (Text name))) (const (stack_pt pt @ stack_rt rt)))
      | _ -> None )
    env.funcs

let expr_br_if (env : Env.t) (stack : val_type list) =
  match stack with
  | [] -> []
  | _hd :: tl ->
    let blocs = Env.get_blocks env in
    List.filter_map
      (fun (bk, name, bt) ->
        match bt with
        | Bt_raw (_, (pt, rt)) ->
          let is_stack_compatible =
            match bk with
            | Env.Block | Env.Func -> S.is_stack_compatible tl (List.rev rt)
            | Env.Loop -> S.is_stack_compatible_param tl (List.rev pt)
          in
          if not is_stack_compatible then None
          else
            let i =
              match bk with
              | Env.Block | Env.Loop -> const @@ Br_if (Text name)
              | Env.Func -> const @@ Br_if (Raw (List.length blocs - 1))
            in
            Some (pair i (const [ S.Pop ]))
        | _ -> None )
      blocs

let random_stack =
  let+ l_vt = list val_type in
  [ S.Whatever l_vt ]

let unreachable : (instr * S.stack_op list) gen =
  pair (const Unreachable) random_stack

let expr_br (env : Env.t) (stack : val_type list) =
  let blocs = Env.get_blocks env in
  List.filter_map
    (fun (bk, name, bt) ->
      match bt with
      | Bt_raw (_, (pt, rt)) ->
        let is_stack_compatible =
          match bk with
          | Env.Block | Env.Func -> S.is_stack_compatible stack (List.rev rt)
          | Env.Loop -> S.is_stack_compatible_param stack (List.rev pt)
        in
        if not is_stack_compatible then None
        else
          let i =
            match bk with
            | Env.Block | Env.Loop -> const @@ Br (Text name)
            | Env.Func -> const @@ Br (Raw (List.length blocs - 1))
          in
          Some (pair i random_stack)
      | _ -> None )
    blocs

let stack_prefix (stack : val_type list) =
  match List.length stack with
  | 0 -> const []
  | len ->
    let+ size = range len in
    List.filteri (fun i _ -> i < size) stack
