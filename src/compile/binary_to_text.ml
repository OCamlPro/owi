(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let convert_indice : Binary.indice -> Text.indice = function i -> Raw i

let convert_block_type : Binary.block_type -> Text.block_type = function
  | Bt_raw (opt, ft) ->
    let opt = Option.map convert_indice opt in
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
  | Memory_init (memidx, dataidx) ->
    let memidx = convert_indice memidx in
    let dataidx = convert_indice dataidx in
    Memory_init (memidx, dataidx)
  | Data_drop id ->
    let id = convert_indice id in
    Data_drop id
  | Elem_drop id ->
    let id = convert_indice id in
    Elem_drop id
  | Select typ -> begin
    match typ with
    | None -> Select None
    | Some [ t ] -> Select (Some [ t ])
    | Some [] | Some (_ :: _ :: _) ->
      (* invalid result arity *)
      (* TODO: maybe we could change the type of Binary.Select to prevent this from happening? *)
      assert false
  end
  | I_unop (nn, op) -> I_unop (nn, op)
  | I_binop (nn, op) -> I_binop (nn, op)
  | I_testop (nn, op) -> I_testop (nn, op)
  | I_relop (nn, op) -> I_relop (nn, op)
  | F_unop (nn, op) -> F_unop (nn, op)
  | F_relop (nn, op) -> F_relop (nn, op)
  | I32_wrap_i64 -> I32_wrap_i64
  | F_reinterpret_i (nn1, nn2) -> F_reinterpret_i (nn1, nn2)
  | I_reinterpret_f (nn1, nn2) -> I_reinterpret_f (nn1, nn2)
  | I64_extend_i32 sx -> I64_extend_i32 sx
  | I64_extend32_s -> I64_extend32_s
  | F32_demote_f64 -> F32_demote_f64
  | I_extend8_s nn -> I_extend8_s nn
  | I_extend16_s nn -> I_extend16_s nn
  | F64_promote_f32 -> F64_promote_f32
  | F_convert_i (nn1, nn2, sx) -> F_convert_i (nn1, nn2, sx)
  | I_trunc_f (nn1, nn2, sx) -> I_trunc_f (nn1, nn2, sx)
  | I_trunc_sat_f (nn1, nn2, sx) -> I_trunc_sat_f (nn1, nn2, sx)
  | Ref_is_null -> Ref_is_null
  | F_binop (nn, op) -> F_binop (nn, op)
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
  | I_load8 (id, nn, sx, memarg) -> I_load8 (convert_indice id, nn, sx, memarg)
  | I_store8 (id, nn, memarg) -> I_store8 (convert_indice id, nn, memarg)
  | I_load16 (id, nn, sx, memarg) -> I_load16 (convert_indice id, nn, sx, memarg)
  | I_store16 (id, nn, memarg) -> I_store16 (convert_indice id, nn, memarg)
  | I64_load32 (id, sx, memarg) -> I64_load32 (convert_indice id, sx, memarg)
  | I64_store32 (id, memarg) -> I64_store32 (convert_indice id, memarg)
  | I_load (id, nn, memarg) -> I_load (convert_indice id, nn, memarg)
  | F_load (id, nn, memarg) -> F_load (convert_indice id, nn, memarg)
  | F_store (id, nn, memarg) -> F_store (convert_indice id, nn, memarg)
  | I_store (id, nn, memarg) -> I_store (convert_indice id, nn, memarg)
  | Memory_copy (id1, id2) ->
    Memory_copy (convert_indice id1, convert_indice id2)
  | Memory_size id -> Memory_size (convert_indice id)
  | Memory_fill id -> Memory_fill (convert_indice id)
  | Memory_grow id -> Memory_grow (convert_indice id)
  | V_ibinop (shape, op) -> V_ibinop (shape, op)
  | Ref_null t -> Ref_null t

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
  Array.map (fun (t : Text.Typedef.t) -> Text.Module.Field.Typedef t) types
  |> Array.to_list

let from_global (global : (Binary.Global.t, Text.Global.Type.t) Origin.t array)
  : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (g : Binary.Global.t) ->
        let init = convert_expr g.init in
        let id = g.id in
        Text.Module.Field.Global { typ = g.typ; init; id }
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ = Text.Import.Type.Global (assigned_name, typ) in
        Text.Module.Field.Import { modul_name; name; typ } )
    global
  |> Array.to_list

let from_table table : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (name, t) -> Text.Module.Field.Table (name, t)
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ = Text.Import.Type.Table (assigned_name, typ) in
        Import { modul_name; name; typ } )
    table
  |> Array.to_list

let from_mem mem : Text.Module.Field.t list =
  Array.map
    (function
      | Origin.Local (name, t) -> Text.Module.Field.Mem (name, t)
      | Imported { modul_name; name; assigned_name; typ } ->
        let typ = Text.Import.Type.Mem (assigned_name, typ) in
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
        Text.Module.Field.Func { type_f; locals = func.locals; body; id }
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
