open Crowbar
open Crowbar.Syntax
open Owi.Text
module S = Type_stack

type num_size =
  | NS8
  | NS16
  | NS32
  | NS64

let num_type =
  choose
    [ const (I32 : num_type)
    ; const (I64 : num_type)
    ; const (F32 : num_type)
    ; const (F64 : num_type)
    ]

let nullable = choose [ const No_null; const (Null : nullable) ]

let heap_type : heap_type Crowbar.gen = const Func_ht
(* TODO: complete this - Extern_ht and others *)

let ref_type = pair nullable heap_type

let table_limits =
  let sup =
    if true then 10 else 100000
    (* TODO: fix max size ? *)
  in
  let* min = range sup in
  let+ max = option (range ~min (sup - min)) in
  { is_i64 = false
  ; min = string_of_int min
  ; max = Option.map string_of_int max
  }

let table_type = pair table_limits ref_type

let sx = choose [ const U; const S ]

let val_type =
  let+ num_type in
  Num_type num_type

let param = pair (const (None : string option)) val_type

let func_type = pair (list param) (list val_type)

let mut = choose [ const (Const : mut); const Var ]

let i32_binop =
  let div =
    let+ sx in
    (Div sx : i32_instr)
  in

  let rem =
    let+ sx in
    (Rem sx : i32_instr)
  in

  let shr =
    let+ sx in
    (Shr sx : i32_instr)
  in

  let+ i =
    choose
      [ const (Add : i32_instr)
      ; const (Sub : i32_instr)
      ; const (Mul : i32_instr)
      ; div
      ; rem
      ; const (And : i32_instr)
      ; const (Or : i32_instr)
      ; const (Xor : i32_instr)
      ; const (Shl : i32_instr)
      ; shr
      ; const (Rotl : i32_instr)
      ; const (Rotr : i32_instr)
      ]
  in
  I32 i

let i64_binop =
  let div =
    let+ sx in
    (Div sx : i64_instr)
  in

  let rem =
    let+ sx in
    (Rem sx : i64_instr)
  in

  let shr =
    let+ sx in
    (Shr sx : i64_instr)
  in
  let+ i =
    choose
      [ const (Add : i64_instr)
      ; const (Sub : i64_instr)
      ; const (Mul : i64_instr)
      ; div
      ; rem
      ; const (And : i64_instr)
      ; const (Or : i64_instr)
      ; const (Xor : i64_instr)
      ; const (Shl : i64_instr)
      ; shr
      ; const (Rotl : i64_instr)
      ; const (Rotr : i64_instr)
      ]
  in
  I64 i

let i32_unop = choose [ const (I32 Clz); const (I32 Ctz); const (I32 Popcnt) ]

let i64_unop = choose [ const (I64 Clz); const (I64 Ctz); const (I64 Popcnt) ]

let i32_testop : Owi.Text.instr gen = const (I32 Eqz)

let i64_testop : Owi.Text.instr gen = const (I64 Eqz)

let i32_relop =
  let ilt =
    let+ sx in
    (Lt sx : Owi.Text.i32_instr)
  in

  let igt =
    let+ sx in
    (Gt sx : Owi.Text.i32_instr)
  in
  let ile =
    let+ sx in
    (Le sx : Owi.Text.i32_instr)
  in
  let ige =
    let+ sx in
    (Ge sx : Owi.Text.i32_instr)
  in
  let+ i =
    choose
      [ const (Eq : Owi.Text.i32_instr)
      ; const (Ne : Owi.Text.i32_instr)
      ; ilt
      ; igt
      ; ile
      ; ige
      ]
  in
  I32 i

let i64_relop =
  let ilt =
    let+ sx in
    (Lt sx : Owi.Text.i64_instr)
  in

  let igt =
    let+ sx in
    (Gt sx : Owi.Text.i64_instr)
  in
  let ile =
    let+ sx in
    (Le sx : Owi.Text.i64_instr)
  in
  let ige =
    let+ sx in
    (Ge sx : Owi.Text.i64_instr)
  in
  let+ i =
    choose
      [ const (Eq : Owi.Text.i64_instr)
      ; const (Ne : Owi.Text.i64_instr)
      ; ilt
      ; igt
      ; ile
      ; ige
      ]
  in
  I64 i

let const_i32 =
  let+ int32 in
  let int32 = Owi.Concrete_i32.of_int32 int32 in
  I32 (Const int32)

let const_i64 =
  let+ int64 in
  let int64 = Owi.Concrete_i64.of_int64 int64 in
  I64 (Const int64)

let const_v128 =
  let* a = int64 in
  let+ b = int64 in
  let v128 = Owi.Concrete_v128.of_i64x2 a b in
  V128 (Const v128)

let i32_wrap_i64 : instr gen = const (I32 Wrap_i64)

let i64_extend_i32 : instr gen =
  let+ sx in
  I64 (Extend_i32 sx)

let i32_extend_32 : instr gen =
  choose [ const (I32 Extend8_s); const (I32 Extend16_s) ]

let i64_extend_64 : instr gen =
  choose
    [ const (I32 Extend8_s); const (I32 Extend16_s); const (I64 Extend32_s) ]

let fbinop_32 : instr gen =
  let+ fbinop =
    choose
      [ const (Add : Owi.Text.f32_instr)
      ; const (Sub : Owi.Text.f32_instr)
      ; const (Mul : Owi.Text.f32_instr)
      ; const (Div : Owi.Text.f32_instr)
      ; const (Min : Owi.Text.f32_instr)
      ; const (Max : Owi.Text.f32_instr)
      ; const (Copysign : Owi.Text.f32_instr)
      ]
  in
  F32 fbinop

let fbinop_64 : instr gen =
  let+ fbinop : Owi.Text.f64_instr =
    choose
      [ const (Add : Owi.Text.f64_instr)
      ; const (Sub : Owi.Text.f64_instr)
      ; const (Mul : Owi.Text.f64_instr)
      ; const (Div : Owi.Text.f64_instr)
      ; const (Min : Owi.Text.f64_instr)
      ; const (Max : Owi.Text.f64_instr)
      ; const (Copysign : Owi.Text.f64_instr)
      ]
  in
  F64 fbinop

let funop_32 : instr gen =
  let+ funop : Owi.Text.f32_instr =
    choose
      [ const (Abs : Owi.Text.f32_instr)
      ; const (Neg : Owi.Text.f32_instr)
      ; const (Sqrt : Owi.Text.f32_instr)
      ; const (Ceil : Owi.Text.f32_instr)
      ; const (Floor : Owi.Text.f32_instr)
      ; const (Trunc : Owi.Text.f32_instr)
      ; const (Nearest : Owi.Text.f32_instr)
      ]
  in
  F32 funop

let funop_64 : instr gen =
  let+ funop =
    choose
      [ const Abs
      ; const Neg
      ; const Sqrt
      ; const Ceil
      ; const Floor
      ; const Trunc
      ; const Nearest
      ]
  in
  F64 funop

let frelop_32 : instr gen =
  let+ frelop : Owi.Text.f32_instr =
    choose
      [ const (Eq : Owi.Text.f32_instr)
      ; const (Ne : Owi.Text.f32_instr)
      ; const (Lt : Owi.Text.f32_instr)
      ; const (Gt : Owi.Text.f32_instr)
      ; const (Le : Owi.Text.f32_instr)
      ; const (Ge : Owi.Text.f32_instr)
      ]
  in
  F32 frelop

let frelop_64 : instr gen =
  let+ frelop =
    choose
      [ const (Owi.Text.Eq : f64_instr)
      ; const Ne
      ; const Lt
      ; const Gt
      ; const Le
      ; const Ge
      ]
  in
  F64 frelop

let const_f32 : instr gen =
  let+ float in
  F32 (Const (Owi.Concrete_f32.of_float float))

let const_f64 : instr gen =
  let+ float in
  F64 (Const (Owi.Concrete_f64.of_float float))

let f32_convert_i32 : instr gen =
  let+ sx in
  F32 (Convert_i (S32, sx))

let f32_convert_i64 : instr gen =
  let+ sx in
  F32 (Convert_i (S64, sx))

let f64_convert_i32 : instr gen =
  let+ sx in
  F64 (Convert_i (S32, sx))

let f64_convert_i64 : instr gen =
  let+ sx in
  F64 (Convert_i (S64, sx))

let i32_trunc_f32 : instr gen =
  let+ sx in
  I32 (Trunc_f (S32, sx))

let i32_trunc_f64 : instr gen =
  let+ sx in
  I32 (Trunc_f (S64, sx))

let i64_trunc_f32 : instr gen =
  let+ sx in
  I64 (Trunc_f (S32, sx))

let i64_trunc_f64 : instr gen =
  let+ sx in
  I64 (Trunc_f (S64, sx))

let i32_trunc_sat_f32 : instr gen =
  let+ sx in
  I64 (Trunc_sat_f (S32, sx))

let i32_trunc_sat_f64 : instr gen =
  let+ sx in
  I32 (Trunc_sat_f (S64, sx))

let i64_trunc_sat_f32 : instr gen =
  let+ sx in
  I64 (Trunc_sat_f (S32, sx))

let i64_trunc_sat_f64 : instr gen =
  let+ sx in
  I64 (Trunc_sat_f (S64, sx))

let f32_demote_f64 : instr gen = const (F32 Demote_f64)

let f64_promote_f32 : instr gen = const (F64 Promote_f32)

let i32_reinterpret_f32 : instr gen = const (I32 (Reinterpret_f S32))

let i64_reinterpret_f64 : instr gen = const (I64 (Reinterpret_f S64))

let f32_reinterpret_i32 : instr gen = const (F32 (Reinterpret_i S32))

let f64_reinterpret_i64 : instr gen = const (F64 (Reinterpret_i S64))

let global ntyp env =
  let globals = Env.get_globals ntyp env ~only_mut:false in
  List.map
    (fun (name, (_, _)) ->
      pair (const (Global (Get (Text name)))) (const [ S.Push (Num_type ntyp) ]) )
    globals

let global_i32 env = global I32 env

let global_i64 env = global I64 env

let global_f32 env = global F32 env

let global_f64 env = global F64 env

let global_set ntyp env =
  let globals = Env.get_globals ntyp env ~only_mut:true in
  List.map
    (fun (name, (_, _)) ->
      pair (const (Global (Set (Text name)))) (const [ S.Pop ]) )
    globals

let global_set_i32 env = global_set I32 env

let global_set_i64 env = global_set I64 env

let global_set_f32 env = global_set F32 env

let global_set_f64 env = global_set F64 env

let local ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    (fun (name, _) ->
      pair (const (Local (Get (Text name)))) (const [ S.Push (Num_type ntyp) ]) )
    locals

let local_i32 env = local I32 env

let local_i64 env = local I64 env

let local_f32 env = local F32 env

let local_f64 env = local F64 env

let local_set ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    (fun (name, _) -> pair (const (Local (Set (Text name)))) (const [ S.Pop ]))
    locals

let local_set_i32 env = local_set I32 env

let local_set_i64 env = local_set I64 env

let local_set_f32 env = local_set F32 env

let local_set_f64 env = local_set F64 env

let local_tee ntyp env =
  let locals = Env.get_locals ntyp env in
  List.map
    (fun (name, _) ->
      pair (const (Local (Tee (Text name)))) (const [ S.Nothing ]) )
    locals

let local_tee_i32 env = local_tee I32 env

let local_tee_i64 env = local_tee I64 env

let local_tee_f32 env = local_tee F32 env

let local_tee_f64 env = local_tee F64 env

let const_of_num_type = function
  | (I32 : num_type) -> const_i32
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
      Owi.Text.Elem.Mode.Active (ind, [ instr ]) )
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
     Memory (Size id) )
    (const [ S.Push (Num_type I32) ])

let memory_grow env : (instr * S.stack_op list) gen =
  pair
    (let+ id = memid env in
     Memory (Grow id) )
    (const [ S.Nothing ])

let memory_copy env : (instr * S.stack_op list) gen =
  pair
    (let+ id1 = memid env
     and+ id2 = memid env in
     Memory (Copy (id1, id2)) )
    (const [ S.Pop; S.Pop; S.Pop ])

let memory_fill env : (instr * S.stack_op list) gen =
  pair
    (let+ id = memid env in
     Memory (Fill id) )
    (const [ S.Pop; S.Pop; S.Pop ])

let memory_init (env : Env.t) =
  List.map
    (fun name ->
      pair
        (let+ id = memid env in
         Memory (Init (id, Text name)) )
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
  let offset = Some (Int64.to_string (Owi.Concrete_i64.to_int64 offset)) in
  let align = Some (string_of_int align) in
  { offset; align }

let i32_load env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  I32 (Load (id, memarg))

let i64_load env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  I64 (Load (id, memarg))

let f32_load env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  F32 (Load (id, memarg))

let f64_load env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  F64 (Load (id, memarg))

let i32_load8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env
  and+ sx in
  I32 (Load8 (id, sx, memarg))

let i32_load16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env
  and+ sx in
  I32 (Load16 (id, sx, memarg))

let i64_load8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env
  and+ sx in
  I64 (Load8 (id, sx, memarg))

let i64_load16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env
  and+ sx in
  I64 (Load16 (id, sx, memarg))

let i64_load32 env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env
  and+ sx in
  I64 (Load32 (id, sx, memarg))

let i32_store env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  I32 (Store (id, memarg))

let i64_store env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  I64 (Store (id, memarg))

let f32_store env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  F32 (Store (id, memarg))

let f64_store env : instr gen =
  let+ memarg = memarg NS64
  and+ id = memid env in
  F64 (Store (id, memarg))

let i32_store8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env in
  I32 (Store8 (id, memarg))

let i64_store8 env : instr gen =
  let+ memarg = memarg NS8
  and+ id = memid env in
  I64 (Store8 (id, memarg))

let i32_store16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env in
  I32 (Store16 (id, memarg))

let i64_store16 env : instr gen =
  let+ memarg = memarg NS16
  and+ id = memid env in
  I64 (Store16 (id, memarg))

let i64_store32 env : instr gen =
  let+ memarg = memarg NS32
  and+ id = memid env in
  I64 (Store32 (id, memarg))

let data_active name =
  let+ inst = const_i32 in
  let exp = [ inst ] in
  Owi.Text.Data.Mode.Active (Some (Text name), exp)

let data_mode (env : Env.t) =
  choose
    ( const Owi.Text.Data.Mode.Passive
    :: List.map (fun id -> data_active id) env.memories )

let data_drop (env : Env.t) =
  List.map
    (fun name -> pair (const (Data (Drop (Text name)))) (const [ S.Nothing ]))
    env.datas

let elem_drop (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair (const (Elem (Drop (Text name)))) (const [ S.Nothing ]) )
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
        (const (Table (Init (Text name_t, Text name_e))))
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
          (const (Table (Copy (Text name_x, Text name_y))))
          (const [ S.Pop; S.Pop; S.Pop ])
      | _ -> pair (const Nop) (const [ S.Nothing ])
      (* TODO: avoid if ... then ... else pair (const (Nop)) (const [ S.Nothing ])
                                               https://github.com/OCamlPro/owi/pull/28#discussion_r1275222846 *)
    in
    [ instr ]

let table_size (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair (const (Table (Size (Text name)))) (const [ S.Push (Num_type I32) ]) )
    env.tables

let table_grow (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair
        (const (Table (Grow (Text name))))
        (const [ S.Pop; S.Pop; S.Push (Num_type I32) ]) )
    env.tables

let table_fill (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair (const (Table (Fill (Text name)))) (const [ S.Pop; S.Pop; S.Pop ]) )
    env.tables

let table_set (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair (const (Table (Set (Text name)))) (const [ S.Pop; S.Pop ]) )
    env.tables

let table_get (env : Env.t) =
  List.map
    (fun (name, _) ->
      pair
        (const (Table (Get (Text name))))
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
