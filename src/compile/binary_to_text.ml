(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let convert_indice : Binary.indice -> Text.indice = function i -> Raw i

let convert_num_type : Binary.num_type -> Text.num_type = function
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128

let convert_heap_type : Binary.heap_type -> Text.heap_type = function
  | Func_ht -> Text.Func_ht
  | Extern_ht -> Text.Extern_ht

let convert_nullable (nullable : Binary.nullable) : Text.nullable =
  match nullable with No_null -> No_null | Null -> Null

let convert_ref_type ((nullable, heap_type) : Binary.ref_type) : Text.ref_type =
  let nullable = convert_nullable nullable in
  let heap_type = convert_heap_type heap_type in
  (nullable, heap_type)

let convert_val_type : Binary.val_type -> Text.val_type = function
  | Num_type t -> Num_type (convert_num_type t)
  | Ref_type t -> Ref_type (convert_ref_type t)

let convert_param ((name, t) : Binary.param) : Text.param =
  (name, convert_val_type t)

let convert_param_type (pt : Binary.param_type) : Text.param_type =
  List.map convert_param pt

let convert_result_type (rt : Binary.result_type) : Text.result_type =
  List.map convert_val_type rt

let convert_func_type ((pt, rt) : Binary.func_type) : Text.func_type =
  let pt = convert_param_type pt in
  let rt = convert_result_type rt in
  (pt, rt)

let convert_nn : Binary.nn -> Text.nn = function S32 -> S32 | S64 -> S64

let convert_ishape : Binary.ishape -> Text.ishape = function
  | I8x16 -> I8x16
  | I16x8 -> I16x8
  | I32x4 -> I32x4
  | I64x2 -> I64x2

let convert_sx : Binary.sx -> Text.sx = function U -> U | S -> S

let convert_iunop : Binary.iunop -> Text.iunop = function
  | Clz -> Clz
  | Ctz -> Ctz
  | Popcnt -> Popcnt

let convert_funop : Binary.funop -> Text.funop = function
  | Abs -> Abs
  | Neg -> Neg
  | Sqrt -> Sqrt
  | Ceil -> Ceil
  | Floor -> Floor
  | Trunc -> Trunc
  | Nearest -> Nearest

let convert_vibinop : Binary.vibinop -> Text.vibinop = function
  | Add -> Add
  | Sub -> Sub

let convert_ibinop : Binary.ibinop -> Text.ibinop = function
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div sx -> Div (convert_sx sx)
  | Rem sx -> Rem (convert_sx sx)
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Shl -> Shl
  | Shr sx -> Shr (convert_sx sx)
  | Rotl -> Rotl
  | Rotr -> Rotr

let convert_fbinop : Binary.fbinop -> Text.fbinop = function
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Min -> Min
  | Max -> Max
  | Copysign -> Copysign

let convert_itestop : Binary.itestop -> Text.itestop = function Eqz -> Eqz

let convert_irelop : Binary.irelop -> Text.irelop = function
  | Eq -> Eq
  | Ne -> Ne
  | Lt sx -> Lt (convert_sx sx)
  | Gt sx -> Gt (convert_sx sx)
  | Le sx -> Le (convert_sx sx)
  | Ge sx -> Ge (convert_sx sx)

let convert_frelop : Binary.frelop -> Text.frelop = function
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge

let convert_memarg (memarg : Binary.memarg) : Text.memarg =
  { offset = memarg.offset; align = memarg.align }

let convert_block_type : Binary.block_type -> Text.block_type = function
  | Bt_raw (opt, ft) ->
    let opt = Option.map convert_indice opt in
    let ft = convert_func_type ft in
    Bt_raw (opt, ft)

let rec convert_instr : Binary.instr -> Text.instr = function
  | Br_table (ids, id) ->
    let ids = Array.map convert_indice ids in
    let id = convert_indice id in
    Br_table (ids, id)
  | Br_if id ->
    let id = convert_indice id in
    Br_if id
  | Br id ->
    let id = convert_indice id in
    Br id
  | Call id ->
    let id = convert_indice id in
    Call id
  | Return_call id ->
    let id = convert_indice id in
    Return_call id
  | Local_set id ->
    let id = convert_indice id in
    Local_set id
  | Local_get id ->
    let id = convert_indice id in
    Local_get id
  | Local_tee id ->
    let id = convert_indice id in
    Local_tee id
  | If_else (id, bt, e1, e2) ->
    let bt = Option.map convert_block_type bt in
    let e1 = convert_expr e1 in
    let e2 = convert_expr e2 in
    If_else (id, bt, e1, e2)
  | Loop (id, bt, e) ->
    let bt = Option.map convert_block_type bt in
    let e = convert_expr e in
    Loop (id, bt, e)
  | Block (id, bt, e) ->
    let bt = Option.map convert_block_type bt in
    let e = convert_expr e in
    Block (id, bt, e)
  | Call_indirect (tbl_i, bt) ->
    let tbl_i = convert_indice tbl_i in
    let bt = convert_block_type bt in
    Call_indirect (tbl_i, bt)
  | Return_call_indirect (tbl_i, bt) ->
    let tbl_i = convert_indice tbl_i in
    let bt = convert_block_type bt in
    Return_call_indirect (tbl_i, bt)
  | Call_ref t ->
    let t = convert_indice t in
    Call_ref t
  | Return_call_ref bt ->
    let bt = convert_block_type bt in
    Return_call_ref bt
  | Global_set id ->
    let id = convert_indice id in
    Global_set id
  | Global_get id ->
    let id = convert_indice id in
    Global_get id
  | Ref_func id ->
    let id = convert_indice id in
    Ref_func id
  | Table_size id ->
    let id = convert_indice id in
    Table_size id
  | Table_get id ->
    let id = convert_indice id in
    Table_get id
  | Table_set id ->
    let id = convert_indice id in
    Table_set id
  | Table_grow id ->
    let id = convert_indice id in
    Table_grow id
  | Table_init (i, i') ->
    let table = convert_indice i in
    let elem = convert_indice i' in
    Table_init (table, elem)
  | Table_fill id ->
    let id = convert_indice id in
    Table_fill id
  | Table_copy (i, i') ->
    let table = convert_indice i in
    let table' = convert_indice i' in
    Table_copy (table, table')
  | Memory_init id ->
    let id = convert_indice id in
    Memory_init id
  | Data_drop id ->
    let id = convert_indice id in
    Data_drop id
  | Elem_drop id ->
    let id = convert_indice id in
    Elem_drop id
  | Select typ -> begin
    match typ with
    | None -> Select None
    | Some [ t ] -> Select (Some [ convert_val_type t ])
    | Some [] | Some (_ :: _ :: _) ->
      (* invalid result arity *)
      (* TODO: maybe we could change the type of Binary.Select to prevent this from happening? *)
      assert false
  end
  | I_unop (nn, op) ->
    let nn = convert_nn nn in
    let op = convert_iunop op in
    I_unop (nn, op)
  | I_binop (nn, op) ->
    let nn = convert_nn nn in
    let op = convert_ibinop op in
    I_binop (nn, op)
  | I_testop (nn, op) ->
    let nn = convert_nn nn in
    let op = convert_itestop op in
    I_testop (nn, op)
  | I_relop (nn, op) ->
    let nn = convert_nn nn in
    let op = convert_irelop op in
    I_relop (nn, op)
  | F_unop (nn, op) ->
    let nn = convert_nn nn in
    let op = convert_funop op in
    F_unop (nn, op)
  | F_relop (nn, op) ->
    let nn = convert_nn nn in
    let op = convert_frelop op in
    F_relop (nn, op)
  | I32_wrap_i64 -> I32_wrap_i64
  | F_reinterpret_i (nn1, nn2) ->
    let nn1 = convert_nn nn1 in
    let nn2 = convert_nn nn2 in
    F_reinterpret_i (nn1, nn2)
  | I_reinterpret_f (nn1, nn2) ->
    let nn1 = convert_nn nn1 in
    let nn2 = convert_nn nn2 in
    I_reinterpret_f (nn1, nn2)
  | I64_extend_i32 sx ->
    let sx = convert_sx sx in
    I64_extend_i32 sx
  | I64_extend32_s -> I64_extend32_s
  | F32_demote_f64 -> F32_demote_f64
  | I_extend8_s nn ->
    let nn = convert_nn nn in
    I_extend8_s nn
  | I_extend16_s nn ->
    let nn = convert_nn nn in
    I_extend16_s nn
  | F64_promote_f32 -> F64_promote_f32
  | F_convert_i (nn1, nn2, sx) ->
    let nn1 = convert_nn nn1 in
    let nn2 = convert_nn nn2 in
    let sx = convert_sx sx in
    F_convert_i (nn1, nn2, sx)
  | I_trunc_f (nn1, nn2, sx) ->
    let nn1 = convert_nn nn1 in
    let nn2 = convert_nn nn2 in
    let sx = convert_sx sx in
    I_trunc_f (nn1, nn2, sx)
  | I_trunc_sat_f (nn1, nn2, sx) ->
    let nn1 = convert_nn nn1 in
    let nn2 = convert_nn nn2 in
    let sx = convert_sx sx in
    I_trunc_sat_f (nn1, nn2, sx)
  | Ref_is_null -> Ref_is_null
  | F_binop (nn, op) ->
    let nn = convert_nn nn in
    let op = convert_fbinop op in
    F_binop (nn, op)
  | F32_const v -> F32_const v
  | F64_const v -> F64_const v
  | I32_const v -> I32_const v
  | I64_const v -> I64_const v
  | V128_const v -> V128_const v
  | Unreachable -> Unreachable
  | Drop -> Drop
  | Nop -> Nop
  | Return -> Return
  | Extern_externalize -> Extern_externalize
  | Extern_internalize -> Extern_internalize
  | I_load8 (nn, sx, memarg) ->
    let nn = convert_nn nn in
    let sx = convert_sx sx in
    let memarg = convert_memarg memarg in
    I_load8 (nn, sx, memarg)
  | I_store8 (nn, memarg) ->
    let nn = convert_nn nn in
    let memarg = convert_memarg memarg in
    I_store8 (nn, memarg)
  | I_load16 (nn, sx, memarg) ->
    let nn = convert_nn nn in
    let sx = convert_sx sx in
    let memarg = convert_memarg memarg in
    I_load16 (nn, sx, memarg)
  | I_store16 (nn, memarg) ->
    let nn = convert_nn nn in
    let memarg = convert_memarg memarg in
    I_store16 (nn, memarg)
  | I64_load32 (sx, memarg) ->
    let sx = convert_sx sx in
    let memarg = convert_memarg memarg in
    I64_load32 (sx, memarg)
  | I64_store32 memarg ->
    let memarg = convert_memarg memarg in
    I64_store32 memarg
  | I_load (nn, memarg) ->
    let nn = convert_nn nn in
    let memarg = convert_memarg memarg in
    I_load (nn, memarg)
  | F_load (nn, memarg) ->
    let nn = convert_nn nn in
    let memarg = convert_memarg memarg in
    F_load (nn, memarg)
  | F_store (nn, memarg) ->
    let nn = convert_nn nn in
    let memarg = convert_memarg memarg in
    F_store (nn, memarg)
  | I_store (nn, memarg) ->
    let nn = convert_nn nn in
    let memarg = convert_memarg memarg in
    I_store (nn, memarg)
  | Memory_copy -> Memory_copy
  | Memory_size -> Memory_size
  | Memory_fill -> Memory_fill
  | Memory_grow -> Memory_grow
  | V_ibinop (shape, op) ->
    let shape = convert_ishape shape in
    let op = convert_vibinop op in
    V_ibinop (shape, op)
  | Ref_null t ->
    let t = convert_heap_type t in
    Ref_null t

and convert_expr (e : Binary.expr Annotated.t) : Text.expr Annotated.t =
  Annotated.map
    (fun (e : Binary.expr) ->
      List.map (fun i -> Annotated.map convert_instr i) e )
    e

let convert_elem_mode : Binary.Elem.Mode.t -> Text.Elem.Mode.t = function
  | Passive -> Passive
  | Declarative -> Declarative
  | Active (opt, e) ->
    let opt = Option.map (fun i -> Text.Raw i) opt in
    let e = convert_expr e in
    Active (opt, e)

let convert_elem : Binary.Elem.t -> Text.Elem.t = function
  | { id; typ; init; mode } ->
    let init = List.map convert_expr init in
    let mode = convert_elem_mode mode in
    let typ = convert_ref_type typ in
    { id; typ; init; mode }

let convert_data_mode : Binary.Data.Mode.t -> Text.Data.Mode.t = function
  | Passive -> Passive
  | Active (i, e) ->
    let e = convert_expr e in
    Active (Some (Raw i), e)

let convert_data : Binary.Data.t -> Text.Data.t = function
  | { id; init; mode } ->
    let mode = convert_data_mode mode in
    { id; init; mode }

let from_types types : Text.Module.Field.t list =
  Array.map
    (fun ((s, ft) : Binary.Typedef.t) ->
      let ft = convert_func_type ft in
      let t = (s, ft) in
      Text.Module.Field.Typedef t )
    types
  |> Array.to_list

let convert_mut : Binary.mut -> Text.mut = function
  | Const -> Const
  | Var -> Var

let convert_global_type ((mut, t) : Binary.Global.Type.t) : Text.Global.Type.t =
  let mut = convert_mut mut in
  let t = convert_val_type t in
  (mut, t)

let convert_limits : Binary.limits -> Text.limits = function
  | { min; max } -> { min; max }

let convert_table_type ((limits, t) : Binary.Table.Type.t) : Text.Table.Type.t =
  let limits = convert_limits limits in
  let t = convert_ref_type t in
  (limits, t)

let from_global (global : (Binary.Global.t, Binary.Global.Type.t) Origin.t array)
  : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (g : Binary.Global.t) ->
        let typ = convert_global_type g.typ in
        let init = convert_expr g.init in
        let id = g.id in
        Text.Module.Field.Global { typ; init; id }
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ = convert_global_type typ in
        let typ = Text.Import.Type.Global (assigned_name, typ) in
        Text.Module.Field.Import { modul_name; name; typ } )
    global
  |> Array.to_list

let from_table table : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (name, t) ->
        let t = convert_table_type t in
        Text.Module.Field.Table (name, t)
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ = convert_table_type typ in
        let typ = Text.Import.Type.Table (assigned_name, typ) in
        Import { modul_name; name; typ } )
    table
  |> Array.to_list

let from_mem mem : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (name, t) ->
        let t = convert_limits t in
        Text.Module.Field.Mem (name, t)
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ = convert_limits typ in
        let typ = Text.Import.Type.Mem (assigned_name, typ) in
        Import { modul_name; name; typ } )
    mem
  |> Array.to_list

let from_func func : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (func : Binary.Func.t) ->
        let type_f = convert_block_type func.type_f in
        let locals = List.map convert_param func.locals in
        let body = convert_expr func.body in
        let id = func.id in
        Text.Module.Field.Func { type_f; locals; body; id }
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ = convert_block_type typ in
        let typ = Text.Import.Type.Func (assigned_name, typ) in
        Text.Module.Field.Import { modul_name; name; typ } )
    func
  |> Array.to_list

let from_elem elem : Text.Module.Field.t list =
  Array.map
    (fun (elem : Binary.Elem.t) ->
      let elem = convert_elem elem in
      Text.Module.Field.Elem elem )
    elem
  |> Array.to_list

let from_data data : Text.Module.Field.t list =
  Array.map
    (fun (data : Binary.Data.t) ->
      let data = convert_data data in
      Text.Module.Field.Data data )
    data
  |> Array.to_list

let from_exports (exports : Binary.Module.Exports.t) : Text.Module.Field.t list
    =
  let global =
    Array.map
      (fun ({ name; id } : Binary.Export.t) ->
        let id = Some (Text.Raw id) in
        Text.Module.Field.Export { name; typ = Global id } )
      exports.global
  in

  let mem =
    Array.map
      (fun ({ name; id } : Binary.Export.t) ->
        let id = Some (Text.Raw id) in
        Text.Module.Field.Export { name; typ = Mem id } )
      exports.mem
  in

  let table =
    Array.map
      (fun ({ name; id } : Binary.Export.t) ->
        let id = Some (Text.Raw id) in
        Text.Module.Field.Export { name; typ = Table id } )
      exports.table
  in

  let func =
    Array.map
      (fun ({ name; id } : Binary.Export.t) ->
        let id = Some (Text.Raw id) in
        Text.Module.Field.Export { name; typ = Func id } )
      exports.func
  in

  Array.to_list global @ Array.to_list mem @ Array.to_list table
  @ Array.to_list func

let from_start = function
  | None -> []
  | Some n -> [ Text.Module.Field.Start (Raw n) ]

let modul
  { Binary.Module.id
  ; types
  ; global
  ; table
  ; mem
  ; func
  ; elem
  ; data
  ; start
  ; exports
  ; _
  } =
  let fields =
    from_types types @ from_global global @ from_table table @ from_mem mem
    @ from_func func @ from_elem elem @ from_data data @ from_exports exports
    @ from_start start
  in
  let imported, locals =
    List.partition_map
      (function
        | Text.Module.Field.Import _ as import -> Either.Left import
        | local -> Either.Right local )
      fields
  in
  let fields = imported @ locals in

  { Text.Module.id; fields }
