(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let convert_indice : Binary.indice -> Text.indice = function i -> Text.Raw i

let convert_heap_type : Binary.heap_type -> Text.heap_type = function
  | TypeUse id -> TypeUse (Raw id)
  | Any_ht -> Any_ht
  | Eq_ht -> Eq_ht
  | I31_ht -> I31_ht
  | Struct_ht -> Struct_ht
  | Array_ht -> Array_ht
  | None_ht -> None_ht
  | Func_ht -> Func_ht
  | NoFunc_ht -> NoFunc_ht
  | Exn_ht -> Exn_ht
  | NoExn_ht -> NoExn_ht
  | Extern_ht -> Extern_ht
  | NoExtern_ht -> NoExtern_ht

let convert_ref_type ((n, ht) : Binary.ref_type) : Text.ref_type =
  (n, convert_heap_type ht)

let convert_val_type : Binary.val_type -> Text.val_type = function
  | Num_type nt -> Num_type nt
  | Ref_type rt -> Ref_type (convert_ref_type rt)

let convert_func_type ((pt, rt) : Binary.func_type) : Text.func_type =
  ( List.map (fun (id, vt) -> (id, convert_val_type vt)) pt
  , List.map convert_val_type rt )

let convert_block_type : Binary.block_type -> Text.block_type = function
  | Binary.Bt_raw (opt, ft) ->
    let opt = Option.map convert_indice opt in
    Text.Bt_raw (opt, convert_func_type ft)

let convert_memarg ({ offset; align } : Binary.memarg) : Text.memarg =
  let offset =
    if Int64.lt 0L offset then Some (Int64.to_string_u offset) else None
  in
  let align =
    assert (Int32.le 0l align);
    Some (Int32.to_string_u (Int32.shl 1l align))
  in
  { offset; align }

let convert_i32_instr : Binary.i32_instr -> Text.i32_instr = function
  | (Binary.Const i : Binary.i32_instr) -> Const i
  | Clz -> Clz
  | Ctz -> Ctz
  | Popcnt -> Popcnt
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div sx -> Div sx
  | Rem sx -> Rem sx
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Shl -> Shl
  | Shr sx -> Shr sx
  | Rotl -> Rotl
  | Rotr -> Rotr
  | Eqz -> Eqz
  | Eq -> Eq
  | Ne -> Ne
  | Lt sx -> Lt sx
  | Gt sx -> Gt sx
  | Le sx -> Le sx
  | Ge sx -> Ge sx
  | Extend8_s -> Extend8_s
  | Extend16_s -> Extend16_s
  | Wrap_i64 -> Wrap_i64
  | Trunc_f (nn, sx) -> Trunc_f (nn, sx)
  | Trunc_sat_f (nn, sx) -> Trunc_sat_f (nn, sx)
  | Reinterpret_f nn -> Reinterpret_f nn
  | Load (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load (indice, memarg)
  | Load8 (indice, sx, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load8 (indice, sx, memarg)
  | Load16 (indice, sx, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load16 (indice, sx, memarg)
  | Store (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store (indice, memarg)
  | Store8 (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store8 (indice, memarg)
  | Store16 (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store16 (indice, memarg)

let convert_i64_instr : Binary.i64_instr -> Text.i64_instr = function
  | Const i -> Const i
  | Clz -> Clz
  | Ctz -> Ctz
  | Popcnt -> Popcnt
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div sx -> Div sx
  | Rem sx -> Rem sx
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Shl -> Shl
  | Shr sx -> Shr sx
  | Rotl -> Rotl
  | Rotr -> Rotr
  | Eqz -> Eqz
  | Eq -> Eq
  | Ne -> Ne
  | Lt sx -> Lt sx
  | Gt sx -> Gt sx
  | Le sx -> Le sx
  | Ge sx -> Ge sx
  | Extend8_s -> Extend8_s
  | Extend16_s -> Extend16_s
  | Extend32_s -> Extend32_s
  | Extend_i32 sx -> Extend_i32 sx
  | Trunc_f (nn, sx) -> Trunc_f (nn, sx)
  | Trunc_sat_f (nn, sx) -> Trunc_sat_f (nn, sx)
  | Reinterpret_f nn -> Reinterpret_f nn
  | Load (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load (indice, memarg)
  | Load8 (indice, sx, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load8 (indice, sx, memarg)
  | Load16 (indice, sx, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load16 (indice, sx, memarg)
  | Load32 (indice, sx, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load32 (indice, sx, memarg)
  | Store (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store (indice, memarg)
  | Store8 (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store8 (indice, memarg)
  | Store16 (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store16 (indice, memarg)
  | Store32 (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store32 (indice, memarg)

let convert_f32_instr : Binary.f32_instr -> Text.f32_instr = function
  | (Const f : Binary.f32_instr) -> Const f
  | Abs -> Abs
  | Neg -> Neg
  | Sqrt -> Sqrt
  | Ceil -> Ceil
  | Floor -> Floor
  | Trunc -> Trunc
  | Nearest -> Nearest
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Min -> Min
  | Max -> Max
  | Copysign -> Copysign
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge
  | Demote_f64 -> Demote_f64
  | Convert_i (nn, sx) -> Convert_i (nn, sx)
  | Reinterpret_i nn -> Reinterpret_i nn
  | Load (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load (indice, memarg)
  | Store (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store (indice, memarg)

let convert_f64_instr : Binary.f64_instr -> Text.f64_instr = function
  | (Const f : Binary.f64_instr) -> Const f
  | Abs -> Abs
  | Neg -> Neg
  | Sqrt -> Sqrt
  | Ceil -> Ceil
  | Floor -> Floor
  | Trunc -> Trunc
  | Nearest -> Nearest
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Min -> Min
  | Max -> Max
  | Copysign -> Copysign
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge
  | Promote_f32 -> Promote_f32
  | Convert_i (nn, sx) -> Convert_i (nn, sx)
  | Reinterpret_i nn -> Reinterpret_i nn
  | Load (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Load (indice, memarg)
  | Store (indice, memarg) ->
    let indice = convert_indice indice in
    let memarg = convert_memarg memarg in
    Store (indice, memarg)

let convert_ref_instr : Binary.ref_instr -> Text.ref_instr = function
  | Null heap_type -> Null (convert_heap_type heap_type)
  | Is_null -> Is_null
  | As_non_null -> As_non_null
  | Func indice ->
    let indice = convert_indice indice in
    Func indice

let convert_local_instr : Binary.local_instr -> Text.local_instr = function
  | Get indice ->
    let indice = convert_indice indice in
    Get indice
  | Set indice ->
    let indice = convert_indice indice in
    Set indice
  | Tee indice ->
    let indice = convert_indice indice in
    Tee indice

let convert_global_instr : Binary.global_instr -> Text.global_instr = function
  | Get indice ->
    let indice = convert_indice indice in
    Get indice
  | Set indice ->
    let indice = convert_indice indice in
    Set indice

let convert_table_instr : Binary.table_instr -> Text.table_instr = function
  | Get indice ->
    let indice = convert_indice indice in
    Get indice
  | Set indice ->
    let indice = convert_indice indice in
    Set indice
  | Size indice ->
    let indice = convert_indice indice in
    Size indice
  | Grow indice ->
    let indice = convert_indice indice in
    Grow indice
  | Fill indice ->
    let indice = convert_indice indice in
    Fill indice
  | Copy (indice1, indice2) ->
    let indice1 = convert_indice indice1 in
    let indice2 = convert_indice indice2 in
    Copy (indice1, indice2)
  | Init (indice1, indice2) ->
    let indice1 = convert_indice indice1 in
    let indice2 = convert_indice indice2 in
    Init (indice1, indice2)

let convert_elem_instr : Binary.elem_instr -> Text.elem_instr = function
  | Drop indice ->
    let indice = convert_indice indice in
    Drop indice

let convert_memory_instr : Binary.memory_instr -> Text.memory_instr = function
  | Size indice ->
    let indice = convert_indice indice in
    Size indice
  | Grow indice ->
    let indice = convert_indice indice in
    Grow indice
  | Fill indice ->
    let indice = convert_indice indice in
    Fill indice
  | Copy (indice1, indice2) ->
    let indice1 = convert_indice indice1 in
    let indice2 = convert_indice indice2 in
    Copy (indice1, indice2)
  | Init (indice1, indice2) ->
    let indice1 = convert_indice indice1 in
    let indice2 = convert_indice indice2 in
    Init (indice1, indice2)

let convert_data_instr : Binary.data_instr -> Text.data_instr = function
  | Drop indice ->
    let indice = convert_indice indice in
    Drop indice

let rec convert_instr : Binary.instr -> Text.instr = function
  | Binary.I32 i -> Text.I32 (convert_i32_instr i)
  | I64 i -> Text.I64 (convert_i64_instr i)
  | F32 i -> Text.F32 (convert_f32_instr i)
  | F64 i -> Text.F64 (convert_f64_instr i)
  | V128 i -> Text.V128 i
  | I8x16 i -> Text.I8x16 i
  | I16x8 i -> Text.I16x8 i
  | I32x4 i -> Text.I32x4 i
  | I64x2 i -> Text.I64x2 i
  | Ref i -> Ref (convert_ref_instr i)
  | Local i -> Local (convert_local_instr i)
  | Global i -> Global (convert_global_instr i)
  | Table i -> Table (convert_table_instr i)
  | Elem i -> Elem (convert_elem_instr i)
  | Memory i -> Memory (convert_memory_instr i)
  | Data i -> Data (convert_data_instr i)
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
  | Br_on_null id ->
    let id = convert_indice id in
    Br_on_null id
  | Br_on_non_null id ->
    let id = convert_indice id in
    Br_on_non_null id
  | Call id ->
    let id = convert_indice id in
    Call id
  | Return_call id ->
    let id = convert_indice id in
    Return_call id
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
  | Select typ ->
    begin match typ with
    | None -> Select None
    | Some [ t ] -> Select (Some [ convert_val_type t ])
    | Some [] | Some (_ :: _ :: _) ->
      (* invalid result arity *)
      (* TODO: maybe we could change the type of Binary.Select to prevent this from happening? *)
      assert false
    end
  | Unreachable -> Unreachable
  | Drop -> Drop
  | Nop -> Nop
  | Return -> Return
  | I_load8 (id, nn, sx, memarg) ->
    I_load8 (convert_indice id, nn, sx, convert_memarg memarg)
  | I_store8 (id, nn, memarg) ->
    I_store8 (convert_indice id, nn, convert_memarg memarg)
  | I_load16 (id, nn, sx, memarg) ->
    I_load16 (convert_indice id, nn, sx, convert_memarg memarg)
  | I_store16 (id, nn, memarg) ->
    I_store16 (convert_indice id, nn, convert_memarg memarg)
  | I64_load32 (id, sx, memarg) ->
    I64_load32 (convert_indice id, sx, convert_memarg memarg)
  | I64_store32 (id, memarg) ->
    I64_store32 (convert_indice id, convert_memarg memarg)
  | I_load (id, nn, memarg) ->
    I_load (convert_indice id, nn, convert_memarg memarg)
  | F_load (id, nn, memarg) ->
    F_load (convert_indice id, nn, convert_memarg memarg)
  | F_store (id, nn, memarg) ->
    F_store (convert_indice id, nn, convert_memarg memarg)
  | I_store (id, nn, memarg) ->
    I_store (convert_indice id, nn, convert_memarg memarg)
  | Memory_copy (id1, id2) ->
    Memory_copy (convert_indice id1, convert_indice id2)
  | Memory_size id -> Memory_size (convert_indice id)
  | Memory_fill id -> Memory_fill (convert_indice id)
  | Memory_grow id -> Memory_grow (convert_indice id)
  | V_ibinop (shape, op) -> V_ibinop (shape, op)
  | Ref_null t -> Ref_null (convert_heap_type t)
  | Ref_i31 -> Ref_i31
  | I31_get_s -> I31_get_s
  | I31_get_u -> I31_get_u
  | Struct_new id -> Struct_new (convert_indice id)
  | Struct_new_default id -> Struct_new_default (convert_indice id)
  | Struct_get (id1, _) -> Struct_get (convert_indice id1, assert false)
  | Struct_get_s (id1, _) -> Struct_get_s (convert_indice id1, assert false)
  | Struct_get_u (id1, _) -> Struct_get_u (convert_indice id1, assert false)
  | Struct_set (id1, _) -> Struct_set (convert_indice id1, assert false)
  | Array_new id -> Array_new (convert_indice id)
  | Array_new_default id -> Array_new_default (convert_indice id)
  | Array_new_fixed (id, n) -> Array_new_fixed (convert_indice id, n)
  | Array_new_data (id1, id2) ->
    Array_new_data (convert_indice id1, convert_indice id2)
  | Array_new_elem (id1, id2) ->
    Array_new_elem (convert_indice id1, convert_indice id2)
  | Array_get id -> Array_get (convert_indice id)
  | Array_get_s id -> Array_get_s (convert_indice id)
  | Array_get_u id -> Array_get_u (convert_indice id)
  | Array_set id -> Array_set (convert_indice id)
  | Array_len -> Array_len
  | Array_fill id -> Array_fill (convert_indice id)
  | Array_copy (id1, id2) -> Array_copy (convert_indice id1, convert_indice id2)
  | Array_init_data (id1, id2) ->
    Array_init_data (convert_indice id1, convert_indice id2)
  | Array_init_elem (id1, id2) ->
    Array_init_elem (convert_indice id1, convert_indice id2)
  | Any_convert_extern -> Any_convert_extern
  | Extern_convert_any -> Extern_convert_any

and convert_expr (e : Binary.expr Annotated.t) : Text.expr =
  List.map (fun i -> convert_instr i.Annotated.raw) e.raw

let convert_elem_mode : Binary.Elem.Mode.t -> Text.Elem.Mode.t = function
  | Binary.Elem.Mode.Passive -> Text.Elem.Mode.Passive
  | Declarative -> Declarative
  | Active (opt, e) ->
    let opt = Option.map (fun i -> Text.Raw i) opt in
    let e = convert_expr e in
    Active (opt, e)

let convert_elem : Binary.Elem.t -> Text.Elem.t = function
  | { Binary.Elem.id; typ; init; mode; explicit_typ } ->
    let init = List.map convert_expr init in
    let mode = convert_elem_mode mode in
    { Text.Elem.id; typ = convert_ref_type typ; init; mode; explicit_typ }

let convert_data_mode : Binary.Data.Mode.t -> Text.Data.Mode.t = function
  | Binary.Data.Mode.Passive -> Text.Data.Mode.Passive
  | Active (i, e) ->
    let e = convert_expr e in
    Active (Some (Raw i), e)

let convert_data : Binary.Data.t -> Text.Data.t = function
  | { Binary.Data.id; init; mode } ->
    let mode = convert_data_mode mode in
    { Text.Data.id; init; mode }

let convert_sub_type Binary.{ final; ids; ct } : Text.sub_type =
  match (final, ids, ct) with
  | false, [], Def_func_t ft ->
    Text.{ final; ids = []; ct = Def_func_t (convert_func_type ft) }
  | _ ->
    Fmt.failwith
      "Uninmplemented: conversion from Binary.sub_type to Text.sub_type"

let from_types types : Text.Module.Field.t list =
  Array.map
    (fun ((id, ft) : Binary.Typedef.t) ->
      Text.Module.Field.Typedef (id, convert_sub_type ft) )
    types
  |> Array.to_list

let from_global (global : (Binary.Global.t, Binary.Global.Type.t) Origin.t array)
  : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local ({ typ = mut, vt; init; id } : Binary.Global.t) ->
        let init = convert_expr init in
        let typ = (mut, convert_val_type vt) in
        Text.Module.Field.Global { typ; init; id }
      | Imported { modul_name; name; assigned_name; typ = mut, vt } ->
        let typ =
          Text.Import.Type.Global (assigned_name, (mut, convert_val_type vt))
        in
        Text.Module.Field.Import { modul_name; name; typ } )
    global
  |> Array.to_list

let convert_table_limits : Binary.Table.Type.limits -> Text.limits = function
  | I64 { min; max } ->
    { is_i64 = true
    ; min = Int64.to_string_u min
    ; max = Option.map Int64.to_string_u max
    }
  | I32 { min; max } ->
    { is_i64 = false
    ; min = Int32.to_string_u min
    ; max = Option.map Int32.to_string_u max
    }

let from_table table : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local Binary.Table.{ id; typ = limits, rt; init } ->
        let init = Option.map convert_expr init in
        Text.Module.Field.Table
          { id; typ = (convert_table_limits limits, convert_ref_type rt); init }
      | Imported { modul_name; name; assigned_name; typ = limits, rt } ->
        let typ =
          Text.Import.Type.Table
            (assigned_name, (convert_table_limits limits, convert_ref_type rt))
        in
        Import { modul_name; name; typ } )
    table
  |> Array.to_list

let convert_mem_limits : Binary.Mem.Type.limits -> Text.limits = function
  | I64 { min; max } ->
    { is_i64 = true
    ; min = Int.to_string min
    ; max = Option.map Int.to_string max
    }
  | I32 { min; max } ->
    { is_i64 = false
    ; min = Int32.to_string_u min
    ; max = Option.map Int32.to_string_u max
    }

let from_mem mem : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (name, t) ->
        Text.Module.Field.Mem (name, convert_mem_limits t)
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ =
          Text.Import.Type.Mem (assigned_name, convert_mem_limits typ)
        in
        Import { modul_name; name; typ } )
    mem
  |> Array.to_list

let from_func func : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (func : Binary.Func.t) ->
        let type_f = convert_block_type func.type_f in
        let body = convert_expr func.body in
        let id = func.id in
        let locals =
          List.map (fun (id, vt) -> (id, convert_val_type vt)) func.locals
        in
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
