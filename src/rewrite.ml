open Syntax
open Types
open Simplified

let find msg (named : 'a Named.t) (indice : Symbolic.indice option) :
  int Result.t =
  match indice with
  | None -> error_s "%s" msg
  | Some indice -> (
    match indice with
    | Symbolic.Arg.Raw i ->
      (* TODO change Indexed.t strucure for that to be more efficient *)
      if not (List.exists (Indexed.has_index i) named.values) then
        error_s "%s %i" msg i
      else Ok i
    | Symbolic name -> (
      match String_map.find_opt name named.named with
      | None -> error_s "%s %s" msg name
      | Some i -> Ok i ) )

let get msg (named : 'a Named.t) indice : 'a Indexed.t Result.t =
  let* i = find msg named indice in
  (* TODO change Named.t structure to make that sensible *)
  match List.nth_opt named.values i with
  | None -> error_s "%s" msg
  | Some v -> Ok v

let find_global (modul : Assigned.t) ~imported_only id : (int * mut) Result.t =
  let* idx = find "unknown global" modul.global id in
  let va = List.find (Indexed.has_index idx) modul.global.values in
  let* mut, _typ =
    match va.value with
    | Imported imported -> Ok imported.desc
    | Local global ->
      if imported_only then Error "unknown global"
      else
        let mut, val_type = global.typ in
        let val_type = Simplified_types.convert_val_type None val_type in
        Ok (mut, val_type)
  in
  Ok (idx, mut)

let rewrite_expr (modul : Assigned.t) (locals : param list)
  (iexpr : Symbolic.expr) : expr Result.t =
  (* block_ids handling *)
  let block_id_to_raw (loop_count, block_ids) id =
    let* id =
      match id with
      | Symbolic.Arg.Symbolic id ->
        let pos = ref (-1) in
        begin
          try
            List.iteri
              (fun i n ->
                if n = Some id then begin
                  pos := i;
                  raise Exit
                end )
              block_ids
          with Exit -> ()
        end;
        if !pos = -1 then Error "unknown label" else Ok !pos
      | Raw id -> Ok id
    in
    (* this is > and not >= because you can `br 0` without any block to target the function *)
    if id > List.length block_ids + loop_count then Error "unknown label"
    else Ok id
  in

  let bt_some_to_raw : Symbolic.block_type -> block_type Result.t = function
    | Symbolic.Arg.Bt_ind ind -> begin
      match get "unknown type" modul.typ (Some ind) with
      | Ok { value = Def_func_t t'; _ } -> Ok t'
      | Error _ as e -> e
      | Ok _ -> Error "TODO: Simplify.bt_some_to_raw"
    end
    | Bt_raw (type_use, t) -> (
      let t = Simplified_types.convert_func_type None t in
      match type_use with
      | None -> Ok t
      | Some ind ->
        (* we check that the explicit type match the type_use, we have to remove parameters names to do so *)
        let* t' =
          match get "unknown type" modul.typ (Some ind) with
          | Ok { value = Def_func_t t'; _ } -> Ok t'
          | Error _ as e -> e
          | Ok _ -> Error "TODO: Simplify.bt_some_to_raw"
        in
        let ok = Simplified_types.equal_func_types t t' in
        if not ok then Error "inline function type" else Ok t )
  in

  let bt_to_raw : Symbolic.block_type option -> block_type option Result.t =
    function
    | None -> Ok None
    | Some bt ->
      let* raw = bt_some_to_raw bt in
      Ok (Some raw)
  in

  let* locals, after_last_assigned_local =
    List.fold_left
      (fun acc ((name, _type) : param) ->
        let* locals, next_free_int = acc in
        match name with
        | None -> Ok (locals, next_free_int + 1)
        | Some name ->
          if String_map.mem name locals then error_s "duplicate local %s" name
          else Ok (String_map.add name next_free_int locals, next_free_int + 1)
        )
      (Ok (String_map.empty, 0))
      locals
  in

  let find_local = function
    | Symbolic.Arg.Raw i ->
      if i >= after_last_assigned_local then Error "unknown local" else Ok i
    | Symbolic name -> (
      match String_map.find_opt name locals with
      | None -> error_s "unknown local %s" name
      | Some id -> Ok id )
  in

  let find_table id = find "unknown table" modul.table id in
  let find_func id = find "unknown function" modul.func id in
  let _find_mem id = find "unknown memory" modul.mem id in
  let find_data id = find "unknown data segment" modul.data id in
  let find_elem id = find "unknown elem segment" modul.elem id in
  let find_type id = find "unknown type" modul.typ id in

  let rec body (loop_count, block_ids) : Symbolic.instr -> instr Result.t =
    function
    | Br_table (ids, id) ->
      let block_id_to_raw = block_id_to_raw (loop_count, block_ids) in
      let* ids = array_map block_id_to_raw ids in
      let* id = block_id_to_raw id in
      ok @@ Br_table (ids, id)
    | Br_if id ->
      let* id = block_id_to_raw (loop_count, block_ids) id in
      ok @@ Br_if id
    | Br id ->
      let* id = block_id_to_raw (loop_count, block_ids) id in
      ok @@ Br id
    | Call id ->
      let* id = find_func (Some id) in
      ok @@ Call id
    | Return_call id ->
      let* id = find_func (Some id) in
      ok @@ Return_call id
    | Local_set id ->
      let* id = find_local id in
      ok @@ Local_set id
    | Local_get id ->
      let* id = find_local id in
      ok @@ Local_get id
    | Local_tee id ->
      let* id = find_local id in
      ok @@ Local_tee id
    | If_else (id, bt, e1, e2) ->
      let* bt = bt_to_raw bt in
      let block_ids = id :: block_ids in
      let* e1 = expr e1 (loop_count, block_ids) in
      let* e2 = expr e2 (loop_count, block_ids) in
      ok @@ If_else (id, bt, e1, e2)
    | Loop (id, bt, e) ->
      let* bt = bt_to_raw bt in
      let* e = expr e (loop_count + 1, id :: block_ids) in
      ok @@ Loop (id, bt, e)
    | Block (id, bt, e) ->
      let* bt = bt_to_raw bt in
      let* e = expr e (loop_count, id :: block_ids) in
      ok @@ Block (id, bt, e)
    | Call_indirect (tbl_i, bt) ->
      let* tbl_i = find_table (Some tbl_i) in
      let* bt = bt_some_to_raw bt in
      ok @@ Call_indirect (tbl_i, bt)
    | Return_call_indirect (tbl_i, bt) ->
      let* tbl_i = find_table (Some tbl_i) in
      let* bt = bt_some_to_raw bt in
      ok @@ Return_call_indirect (tbl_i, bt)
    | Call_ref bt ->
      let* bt = bt_some_to_raw bt in
      ok @@ Call_ref bt
    | Return_call_ref bt ->
      let* bt = bt_some_to_raw bt in
      ok @@ Return_call_ref bt
    | Global_set id -> begin
      let* idx, mut = find_global modul ~imported_only:false (Some id) in
      match mut with
      | Const -> Error "global is immutable"
      | Var -> ok @@ Global_set idx
    end
    | Global_get id ->
      let* idx, _mut = find_global modul ~imported_only:false (Some id) in
      ok @@ Global_get idx
    | Ref_func id ->
      let* id = find_func (Some id) in
      ok @@ Ref_func id
    | Table_size id ->
      let* id = find_table (Some id) in
      ok @@ Table_size id
    | Table_get id ->
      let* id = find_table (Some id) in
      ok @@ Table_get id
    | Table_set id ->
      let* id = find_table (Some id) in
      ok @@ Table_set id
    | Table_grow id ->
      let* id = find_table (Some id) in
      ok @@ Table_grow id
    | Table_init (i, i') ->
      let* table = find_table (Some i) in
      let* elem = find_elem (Some i') in
      ok @@ Table_init (table, elem)
    | Table_fill id ->
      let* id = find_table (Some id) in
      ok @@ Table_fill id
    | Table_copy (i, i') ->
      let* table = find_table (Some i) in
      let* table' = find_table (Some i') in
      ok @@ Table_copy (table, table')
    | Memory_init id ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else
        let* id = find_data (Some id) in
        ok @@ Memory_init id
    | Data_drop id ->
      let* id = find_data (Some id) in
      ok @@ Data_drop id
    | Elem_drop id ->
      let* id = find_elem (Some id) in
      ok @@ Elem_drop id
    (* TODO: should we check alignment or memory existence first ? is it tested in the reference implementation ? *)
    | I_load8 (nn, sx, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else if memarg.align >= 1 then
        Error "alignment must not be larger than natural"
      else Ok (I_load8 (nn, sx, memarg))
    | I_store8 (nn, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else if memarg.align >= 1 then
        Error "alignment must not be larger than natural"
      else ok @@ I_store8 (nn, memarg)
    | I_load16 (nn, sx, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else if memarg.align >= 2 then
        Error "alignment must not be larger than natural"
      else ok @@ I_load16 (nn, sx, memarg)
    | I_store16 (nn, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else if memarg.align >= 2 then
        Error "alignment must not be larger than natural"
      else ok @@ I_store16 (nn, memarg)
    | I64_load32 (nn, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else if memarg.align >= 4 then
        Error "alignment must not be larger than natural"
      else ok @@ I64_load32 (nn, memarg)
    | I64_store32 memarg ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else if memarg.align >= 4 then
        Error "alignment must not be larger than natural"
      else ok @@ I64_store32 memarg
    | I_load (nn, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else
        let max_allowed = match nn with S32 -> 4 | S64 -> 8 in
        if memarg.align >= max_allowed then
          Error "alignment must not be larger than natural"
        else ok @@ I_load (nn, memarg)
    | F_load (nn, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else
        let max_allowed = match nn with S32 -> 4 | S64 -> 8 in
        if memarg.align >= max_allowed then
          Error "alignment must not be larger than natural"
        else ok @@ F_load (nn, memarg)
    | F_store (nn, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else
        let max_allowed = match nn with S32 -> 4 | S64 -> 8 in
        if memarg.align >= max_allowed then
          Error "alignment must not be larger than natural"
        else ok @@ F_store (nn, memarg)
    | I_store (nn, memarg) ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else
        let max_allowed = match nn with S32 -> 4 | S64 -> 8 in
        if memarg.align >= max_allowed then
          Error "alignment must not be larger than natural"
        else ok @@ I_store (nn, memarg)
    | Memory_copy ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else Ok Memory_copy
    | Memory_size ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else Ok Memory_size
    | Memory_fill ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else Ok Memory_fill
    | Memory_grow ->
      if List.length modul.mem.values < 1 then Error "unknown memory 0"
      else Ok Memory_grow
    | Select typ -> begin
      match typ with
      | None -> ok @@ Select None
      | Some [ t ] ->
        ok @@ Select (Some [ Simplified_types.convert_val_type None t ])
      | Some [] | Some (_ :: _ :: _) -> Error "invalid result arity"
    end
    | Array_new_default id ->
      let* id = find_type (Some id) in
      ok @@ Array_new_default id
    | Array_set id ->
      let* id = find_type (Some id) in
      ok @@ Array_set id
    | Array_get id ->
      let* id = find_type (Some id) in
      ok @@ Array_set id
    | I_unop (nn, o) -> ok @@ I_unop (nn, o)
    | I_binop (nn, o) -> ok @@ I_binop (nn, o)
    | I_testop (nn, o) -> ok @@ I_testop (nn, o)
    | I_relop (nn, o) -> ok @@ I_relop (nn, o)
    | F_unop (nn, o) -> ok @@ F_unop (nn, o)
    | F_relop (nn, o) -> ok @@ F_relop (nn, o)
    | I32_wrap_i64 -> ok @@ I32_wrap_i64
    | Ref_null heap_type ->
      ok @@ Ref_null (Simplified_types.convert_heap_type None heap_type)
    | F_reinterpret_i (nn, nn') -> ok @@ F_reinterpret_i (nn, nn')
    | I_reinterpret_f (nn, nn') -> ok @@ I_reinterpret_f (nn, nn')
    | I64_extend_i32 sx -> ok @@ I64_extend_i32 sx
    | I64_extend32_s -> ok @@ I64_extend32_s
    | F32_demote_f64 -> ok @@ F32_demote_f64
    | I_extend8_s nn -> ok @@ I_extend8_s nn
    | I_extend16_s nn -> ok @@ I_extend16_s nn
    | F64_promote_f32 -> ok @@ F64_promote_f32
    | F_convert_i (nn, nn', sx) -> ok @@ F_convert_i (nn, nn', sx)
    | I_trunc_f (nn, nn', sx) -> ok @@ I_trunc_f (nn, nn', sx)
    | I_trunc_sat_f (nn, nn', sx) -> ok @@ I_trunc_sat_f (nn, nn', sx)
    | Ref_is_null -> Ok Ref_is_null
    | F_binop (nn, o) -> ok @@ F_binop (nn, o)
    | F32_const f -> ok @@ F32_const f
    | F64_const f -> ok @@ F64_const f
    | I32_const i -> ok @@ I32_const i
    | I64_const i -> ok @@ I64_const i
    | Unreachable -> Ok Unreachable
    | Drop -> Ok Drop
    | Nop -> Ok Nop
    | Return -> Ok Return
    | I31_new -> Ok I31_new
    | I31_get_s -> Ok I31_get_s
    | I31_get_u -> Ok I31_get_u
    | Array_len -> Ok Array_len
    | Ref_as_non_null -> Ok Ref_as_non_null
    | Extern_externalize -> Ok Extern_externalize
    | Extern_internalize -> Ok Extern_internalize
    | Ref_eq -> Ok Ref_eq
    | Br_on_cast (i, null, ht) ->
      let* i = find_type (Some i) in
      let ht = Simplified_types.convert_heap_type None ht in
      ok @@ Br_on_cast (i, null, ht)
    | Br_on_cast_fail (i, null, ht) ->
      let* i = find_type (Some i) in
      let ht = Simplified_types.convert_heap_type None ht in
      ok @@ Br_on_cast_fail (i, null, ht)
    | Struct_new_default i ->
      let* i = find_type (Some i) in
      ok @@ Struct_new_default i
    | Ref_cast (null, ht) ->
      let ht = Simplified_types.convert_heap_type None ht in
      ok @@ Ref_cast (null, ht)
    | Ref_test (null, ht) ->
      let ht = Simplified_types.convert_heap_type None ht in
      ok @@ Ref_test (null, ht)
    | ( Array_new_data _ | Array_new _ | Array_new_elem _ | Array_new_fixed _
      | Array_get_u _ | Struct_get _ | Struct_get_s _ | Struct_set _
      | Struct_new _ | Br_on_non_null _ | Br_on_null _ ) as i ->
      Log.debug "TODO (Rewrite.body) %a@\n" Symbolic.Pp.instr i;
      Ok Nop
  and expr (e : Symbolic.expr) (loop_count, block_ids) : expr Result.t =
    list_map (fun i -> body (loop_count, block_ids) i) e
  in
  expr iexpr (0, [])

let rewrite_const_expr (modul : Assigned.t) (expr : Symbolic.expr) :
  Const.expr Result.t =
  let const_instr (instr : Symbolic.instr) : Const.instr Result.t =
    let open Const in
    match instr with
    | Symbolic.I32_const v -> ok @@ I32_const v
    | I64_const v -> ok @@ I64_const v
    | F32_const v -> ok @@ F32_const v
    | F64_const v -> ok @@ F64_const v
    | Ref_null v -> ok @@ Ref_null (Simplified_types.convert_heap_type None v)
    | Ref_func f ->
      let* f = find "unknown function" modul.func (Some f) in
      ok @@ Ref_func f
    | Global_get id -> begin
      let* idx, mut = find_global modul ~imported_only:true (Some id) in
      match mut with
      | Const -> ok @@ Global_get idx
      | Var -> Error "constant expression required"
    end
    | Array_new t ->
      let* t = find "unknown type" modul.typ (Some t) in
      ok @@ Array_new t
    | Array_new_default t ->
      let* t = find "unknown type" modul.typ (Some t) in
      ok @@ Array_new_default t
    | Symbolic.I31_new -> Ok I31_new
    | i ->
      error
      @@ Format.asprintf "constant expression required, got %a"
           Symbolic.Pp.instr i
  in
  list_map const_instr expr

let rewrite_block_type (modul : Assigned.t) (block_type : Symbolic.block_type) :
  block_type Result.t =
  match block_type with
  | Symbolic.Arg.Bt_ind id ->
    let* t =
      match get "unknown type" modul.typ (Some id) with
      | Ok { value = Def_func_t t'; _ } -> Ok t'
      | Error _ as e -> e
      | Ok _ -> Error "TODO: Simplify.bt_some_to_raw"
    in
    Ok t
  | Bt_raw (_, func_type) ->
    Ok (Simplified_types.convert_func_type None func_type)

let rewrite_global (modul : Assigned.t) (global : Symbolic.global) :
  global Result.t =
  let* init = rewrite_const_expr modul global.init in
  let mut, val_type = global.typ in
  let typ = (mut, Simplified_types.convert_val_type None val_type) in
  let global = { id = global.id; init; typ } in
  Ok global

let rewrite_elem (modul : Assigned.t) (elem : Symbolic.elem) : elem Result.t =
  let* (mode : elem_mode) =
    match elem.mode with
    | Elem_declarative -> Ok Elem_declarative
    | Elem_passive -> Ok Elem_passive
    | Elem_active (indice, expr) ->
      let* indice = find "unknown table" modul.table indice in
      let* expr = rewrite_const_expr modul expr in
      ok @@ Elem_active (Some indice, expr)
  in
  let* init = list_map (rewrite_const_expr modul) elem.init in
  let typ = Simplified_types.convert_ref_type None elem.typ in
  let elem = { init; mode; id = elem.id; typ } in
  Ok elem

let rewrite_data (modul : Assigned.t) (data : Symbolic.data) : data Result.t =
  let* mode =
    match data.mode with
    | Data_passive -> Ok Data_passive
    | Data_active (indice, expr) ->
      let* indice = find "unknown memory" modul.mem indice in
      let* expr = rewrite_const_expr modul expr in
      ok @@ Data_active (Some indice, expr)
  in

  let data : data = { mode; id = data.id; init = data.init } in
  Ok data

let rewrite_export msg named (exports : Grouped.opt_export list) :
  export list Result.t =
  list_map
    (fun { Grouped.name; id } ->
      let* id =
        match id with Curr id -> Ok id | Indice id -> find msg named (Some id)
      in
      Ok ({ name; id } : export) )
    exports

let rewrite_exports (modul : Assigned.t) (exports : Grouped.opt_exports) :
  exports Result.t =
  let* global = rewrite_export "unknown global" modul.global exports.global in
  let* mem = rewrite_export "unknown memory" modul.mem exports.mem in
  let* table = rewrite_export "unknown table" modul.table exports.table in
  let* func = rewrite_export "unknown function" modul.func exports.func in
  let e : exports = { global; mem; table; func } in
  Ok e

let rewrite_func (modul : Assigned.t) (func : Symbolic.func) : func Result.t =
  let* type_f = rewrite_block_type modul func.type_f in
  let params, _ = type_f in
  let locals = List.map (Simplified_types.convert_param None) func.locals in
  let* body = rewrite_expr modul (params @ locals) func.body in
  let func = { body; type_f; id = func.id; locals } in
  Ok func

let rewrite_import (f : 'a -> 'b Result.t) (import : 'a Imported.t) :
  'b Imported.t Result.t =
  let* desc = f import.desc in
  Ok { import with desc }

let rewrite_runtime f g r =
  match r with
  | Runtime.Local v ->
    let* v = f v in
    ok @@ Runtime.Local v
  | Imported i ->
    let* i = g i in
    ok @@ Runtime.Imported i

let rewrite_named f named =
  let* values =
    list_map
      (fun ind ->
        let* value = f ind.Indexed.value in
        Ok { ind with value } )
      named.Named.values
  in
  Ok { named with Named.values }

let modul (modul : Assigned.t) : modul Result.t =
  Log.debug "rewriting    ...@\n";
  let* (global : (global, global_type) Runtime.t Named.t) =
    let* { Named.named; values } =
      rewrite_named (rewrite_runtime (rewrite_global modul) ok) modul.global
    in
    let values = List.rev values in
    let global : (global, global_type) Runtime.t Named.t =
      { Named.named; values }
    in
    Ok global
  in
  let* elem = rewrite_named (rewrite_elem modul) modul.elem in
  let* data = rewrite_named (rewrite_data modul) modul.data in
  let* exports = rewrite_exports modul modul.exports in
  let* func =
    let import = rewrite_import (rewrite_block_type modul) in
    let runtime = rewrite_runtime (rewrite_func modul) import in
    rewrite_named runtime modul.func
  in
  let* start =
    match modul.start with
    | None -> Ok None
    | Some start -> (
      let* idx = find "unknown function" func (Some start) in
      let va = List.find (Indexed.has_index idx) func.Named.values in
      let param_typ, result_typ =
        match va.value with
        | Local func -> func.type_f
        | Imported imported -> imported.desc
      in
      match (param_typ, result_typ) with
      | [], [] -> Ok (Some idx)
      | _, _ -> Error "start function" )
  in

  let modul : modul =
    { id = modul.id
    ; mem = modul.mem
    ; table = modul.table
    ; global
    ; elem
    ; data
    ; exports
    ; func
    ; start
    }
  in
  Ok modul
