open Syntax
open Types
open Simplified

type type_check = Symbolic.indice * Symbolic.func_type

type opt_ind =
  | Curr of int
  | Indice of Symbolic.indice

type opt_export =
  { name : string
  ; id : opt_ind
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

let curr_id (curr : int) (i : Symbolic.indice option) =
  match i with None -> Curr (pred curr) | Some id -> Indice id

type grouped_module =
  { id : string option
  ; typ : Symbolic.type_def list
  ; function_type : Symbolic.func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Symbolic.global, global_type) runtime indexed list
  ; table : (table, table_type) runtime indexed list
  ; mem : (mem, limits) runtime indexed list
  ; func : (Symbolic.func, Symbolic.block_type) runtime indexed list
  ; elem : Symbolic.elem indexed list
  ; data : Symbolic.data indexed list
  ; exports : opt_exports
  ; start : Symbolic.indice option
  }

let imp (import : Symbolic.import) (assigned_name, desc) : 'a imp =
  { modul = import.modul; name = import.name; assigned_name; desc }

let empty_module id =
  { id
  ; typ = []
  ; function_type = []
  ; type_checks = []
  ; global = []
  ; table = []
  ; mem = []
  ; func = []
  ; elem = []
  ; data = []
  ; exports = { global = []; table = []; mem = []; func = [] }
  ; start = None
  }

type curr =
  { global : int
  ; table : int
  ; mem : int
  ; func : int
  ; elem : int
  ; data : int
  }

let init_curr = { global = 0; table = 0; mem = 0; func = 0; elem = 0; data = 0 }

let add_global value (fields : grouped_module) (curr : curr) =
  let index = curr.global in
  ( { fields with global = { index; value } :: fields.global }
  , { curr with global = succ curr.global } )

let add_table value (fields : grouped_module) (curr : curr) =
  let index = curr.table in
  ( { fields with table = { index; value } :: fields.table }
  , { curr with table = succ curr.table } )

let add_mem value (fields : grouped_module) (curr : curr) =
  let index = curr.mem in
  ( { fields with mem = { index; value } :: fields.mem }
  , { curr with mem = succ curr.mem } )

let add_func value (fields : grouped_module) (curr : curr) =
  let index = curr.func in
  ( { fields with func = { index; value } :: fields.func }
  , { curr with func = succ curr.func } )

let add_elem value (fields : grouped_module) (curr : curr) =
  let index = curr.elem in
  ( { fields with elem = { index; value } :: fields.elem }
  , { curr with elem = succ curr.elem } )

let add_data value (fields : grouped_module) (curr : curr) =
  let index = curr.data in
  ( { fields with data = { index; value } :: fields.data }
  , { curr with data = succ curr.data } )

let check_limit { min; max } =
  match max with
  | None -> Ok ()
  | Some max ->
    if min > max then Error "size minimum must not be greater than maximum"
    else Ok ()

let convert_heap_type : Symbolic.heap_type -> heap_type = function
  | Symbolic.Any_ht -> Any_ht
  | None_ht -> None_ht
  | Eq_ht -> Eq_ht
  | I31_ht -> I31_ht
  | Struct_ht -> Struct_ht
  | Array_ht -> Array_ht
  | Func_ht -> Func_ht
  | No_func_ht -> No_func_ht
  | Extern_ht -> Extern_ht
  | No_extern_ht -> No_extern_ht
  | Def_ht (Raw i) -> Def_ht i
  | Def_ht (Symbolic _i) ->
    (* TODO *)
    let i = 42 in
    Def_ht i

let convert_ref_type : Symbolic.ref_type -> ref_type =
 fun (null, heap_type) -> (null, convert_heap_type heap_type)

let convert_val_type : Symbolic.val_type -> val_type = function
  | Symbolic.Num_type t -> Num_type t
  | Ref_type rt -> Ref_type (convert_ref_type rt)

let convert_param : Symbolic.param -> param =
 fun (n, t) -> (n, convert_val_type t)

let convert_pt : Symbolic.param_type -> param_type =
 fun l -> List.map convert_param l

let convert_rt : Symbolic.result_type -> result_type =
 fun l -> List.map convert_val_type l

let convert_func_type : Symbolic.func_type -> func_type =
 fun (pt, rt) -> (convert_pt pt, convert_rt rt)

let convert_storage_type : Symbolic.storage_type -> storage_type = function
  | Val_storage_t val_type -> Val_storage_t (convert_val_type val_type)
  | Val_packed_t packed_type -> Val_packed_t packed_type

let convert_field_type : Symbolic.field_type -> field_type =
 fun (mut, storage_type) -> (mut, convert_storage_type storage_type)

let convert_struct_field : Symbolic.struct_field -> struct_field =
 fun (id, types) -> (id, List.map convert_field_type types)

let convert_struct_type : Symbolic.struct_type -> struct_type =
 fun fields -> List.map convert_struct_field fields

let convert_str : Symbolic.str_type -> str_type = function
  | Def_func_t func_t -> Def_func_t (convert_func_type func_t)
  | Def_array_t field_t -> Def_array_t (convert_field_type field_t)
  | Def_struct_t struct_t -> Def_struct_t (convert_struct_type struct_t)

let convert_table_type : Symbolic.table_type -> table_type =
 fun (limits, ref_type) -> (limits, convert_ref_type ref_type)

let group (modul : Symbolic.modul) : grouped_module Result.t =
  let add ((fields : grouped_module), curr) field :
    (grouped_module * curr) Result.t =
    match field with
    | Symbolic.MType typ ->
      let typ = typ @ fields.typ in
      ok @@ ({ fields with typ }, curr)
    | MGlobal global -> ok @@ add_global (Local global) fields curr
    | MImport ({ desc = Import_global (a, (mut, val_type)); _ } as import) ->
      let b = (mut, convert_val_type val_type) in
      let imported = imp import (a, b) in
      ok @@ add_global (Imported imported) fields curr
    | MExport { name; desc = Export_global id } ->
      let id = curr_id curr.global id in
      let exports =
        { fields.exports with global = { name; id } :: fields.exports.global }
      in
      ok ({ fields with exports }, curr)
    | MTable table ->
      let _, (limits, _) = table in
      let* () = check_limit limits in
      let id, table_type = table in
      let table = (id, convert_table_type table_type) in
      ok @@ add_table (Local table) fields curr
    | MImport ({ desc = Import_table (id, table_type); _ } as import) ->
      let table_type = convert_table_type table_type in
      let imported = imp import (id, table_type) in
      ok @@ add_table (Imported imported) fields curr
    | MExport { name; desc = Export_table id } ->
      let id = curr_id curr.table id in
      let exports =
        { fields.exports with table = { name; id } :: fields.exports.table }
      in
      ok ({ fields with exports }, curr)
    | MMem ((_, limits) as mem) ->
      let* () =
        if limits.min > 65536 then
          Error "memory size must be at most 65536 pages (4GiB)"
        else Ok ()
      in
      let* () =
        match limits.max with
        | Some max when max > 65536 ->
          Error "memory size must be at most 65536 pages (4GiB)"
        | Some _ | None -> Ok ()
      in
      let* () = check_limit limits in
      ok @@ add_mem (Local mem) fields curr
    | MImport ({ desc = Import_mem (id, limits); _ } as import) ->
      let imported = imp import (id, limits) in
      ok @@ add_mem (Imported imported) fields curr
    | MExport { name; desc = Export_mem id } ->
      let id = curr_id curr.mem id in
      let exports =
        { fields.exports with mem = { name; id } :: fields.exports.mem }
      in
      Ok ({ fields with exports }, curr)
    | MFunc func ->
      let function_type, type_checks =
        match func.type_f with
        | Bt_ind _ -> (fields.function_type, fields.type_checks)
        | Bt_raw (id, typ) ->
          let type_checks =
            match id with
            | None -> fields.type_checks
            | Some id -> (id, typ) :: fields.type_checks
          in
          (typ :: fields.function_type, type_checks)
      in
      let index = curr.func in
      let func = { value = Local func; index } :: fields.func in
      Ok
        ( { fields with func; function_type; type_checks }
        , { curr with func = succ curr.func } )
    | MImport ({ desc = Import_func (a, b); _ } as import) ->
      let imported = imp import (a, b) in
      ok @@ add_func (Imported imported) fields curr
    | MExport { name; desc = Export_func id } ->
      let id = curr_id curr.func id in
      let exports =
        { fields.exports with func = { name; id } :: fields.exports.func }
      in
      Ok ({ fields with exports }, curr)
    | MElem elem ->
      let mode =
        match elem.mode with
        | (Symbolic.Elem_passive | Elem_declarative) as mode -> mode
        | Elem_active (id, expr) ->
          let id = Option.value id ~default:(Raw (curr.table - 1)) in
          Elem_active (Some id, expr)
      in
      ok @@ add_elem { elem with mode } fields curr
    | MData data ->
      let mode =
        match data.mode with
        | Data_passive -> Symbolic.Data_passive
        | Data_active (id, expr) ->
          let id = Option.value id ~default:(Raw (curr.mem - 1)) in
          Data_active (Some id, expr)
      in
      let data : Symbolic.data = { id = data.id; init = data.init; mode } in
      ok @@ add_data data fields curr
    | MStart start -> Ok ({ fields with start = Some start }, curr)
  in
  let* modul, _curr =
    list_fold_left add (empty_module modul.id, init_curr) modul.fields
  in
  Ok modul

module StrType = struct
  type t = str_type

  let compare = compare
end

module TypeMap = Map.Make (StrType)

let equal_func_types (a : func_type) (b : func_type) : bool =
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

type assigned_module =
  { id : string option
  ; typ : str_type Named.t
  ; global : (Symbolic.global, global_type) runtime Named.t
  ; table : (table, table_type) runtime Named.t
  ; mem : (mem, limits) runtime Named.t
  ; func : (Symbolic.func, Symbolic.block_type) runtime Named.t
  ; elem : Symbolic.elem Named.t
  ; data : Symbolic.data Named.t
  ; exports : opt_exports
  ; start : Symbolic.indice option
  }

module Assign_indicies : sig
  val run : grouped_module -> assigned_module Result.t
end = struct
  type type_acc =
    { declared_types : str_type indexed list
    ; func_types : str_type indexed list
    ; named_types : int StringMap.t
    ; last_assigned_int : int
    ; all_types : int TypeMap.t
    }

  let assign_types (modul : grouped_module) : str_type Named.t =
    let assign_type
      { declared_types; func_types; named_types; last_assigned_int; all_types }
      (name, sub_type) : type_acc =
      let last_assigned_int, declared_types, named_types, all_types =
        match sub_type with
        | _final, _indices, str_type ->
          let str_type = convert_str str_type in
          let id = last_assigned_int in
          let last_assigned_int = succ last_assigned_int in
          let declared_types =
            { index = id; value = str_type } :: declared_types
          in
          let named_types =
            match name with
            | None -> named_types
            | Some name -> StringMap.add name id named_types
          in
          let all_types = TypeMap.add str_type id all_types in
          (last_assigned_int, declared_types, named_types, all_types)
      in

      (* Is there something to do/check when a type is already declared ? *)
      { declared_types; func_types; named_types; last_assigned_int; all_types }
    in

    let empty_acc =
      { declared_types = []
      ; func_types = []
      ; named_types = StringMap.empty
      ; last_assigned_int = 0
      ; all_types = TypeMap.empty
      }
    in
    let acc = List.fold_left assign_type empty_acc (List.rev modul.typ) in
    let assign_heap_type
      ({ func_types; named_types = _; last_assigned_int; all_types; _ } as acc)
      typ =
      let typ = Def_func_t (convert_func_type typ) in
      match TypeMap.find_opt typ all_types with
      | Some _id -> acc
      | None ->
        let id = last_assigned_int in
        let last_assigned_int = last_assigned_int + 1 in
        let all_types = TypeMap.add typ id all_types in
        let func_types =
          match typ with
          | Def_func_t _ftype -> { index = id; value = typ } :: func_types
          | Def_array_t (_mut, _storage_type) -> func_types
          | Def_struct_t _ -> func_types
        in
        { acc with func_types; last_assigned_int; all_types }
    in
    let acc =
      List.fold_left assign_heap_type acc (List.rev modul.function_type)
    in
    let values = List.rev acc.declared_types @ List.rev acc.func_types in
    { values; named = acc.named_types }

  let get_runtime_name (get_name : 'a -> string option) (elt : ('a, 'b) runtime)
    : string option =
    match elt with
    | Local v -> get_name v
    | Imported { assigned_name; _ } -> assigned_name

  let name kind ~get_name values =
    let assign_one (named : int StringMap.t) (elt : _ indexed) =
      match get_name elt.value with
      | None -> Ok named
      | Some name ->
        if StringMap.mem name named then error_s "duplicate %s %s" kind name
        else ok @@ StringMap.add name elt.index named
    in
    let* named = list_fold_left assign_one StringMap.empty values in
    Ok { Named.values; named }

  let check_type_id (types : str_type Named.t) (check : type_check) =
    let id, func_type = check in
    let* id =
      match id with
      | Raw i -> Ok i
      | Symbolic name -> (
        match StringMap.find_opt name types.named with
        | None -> error_s "internal error: can't find type with name %s" name
        | Some t -> Ok t )
    in
    (* TODO more efficient version of that *)
    match List.find_opt (fun v -> v.index = id) types.values with
    | None -> Error "unknown type"
    | Some { value = Def_func_t func_type'; _ } ->
      let func_type = convert_func_type func_type in
      if not (equal_func_types func_type func_type') then
        Error "inline function type"
      else Ok ()
    | Some _ -> Error "TODO: Simplify.check_type_id"

  let run (modul : grouped_module) : assigned_module Result.t =
    let typ = assign_types modul in
    let* global =
      name "global"
        ~get_name:(get_runtime_name (fun ({ id; _ } : Symbolic.global) -> id))
        modul.global
    in
    let* table =
      name "table"
        ~get_name:(get_runtime_name (fun ((id, _) : table) -> id))
        modul.table
    in
    let* mem =
      name "mem"
        ~get_name:(get_runtime_name (fun ((id, _) : mem) -> id))
        modul.mem
    in
    let* func =
      name "func"
        ~get_name:(get_runtime_name (fun ({ id; _ } : Symbolic.func) -> id))
        modul.func
    in
    let* elem =
      name "elem" ~get_name:(fun (elem : Symbolic.elem) -> elem.id) modul.elem
    in
    let* data =
      name "data" ~get_name:(fun (data : Symbolic.data) -> data.id) modul.data
    in
    let* () = list_iter (check_type_id typ) modul.type_checks in
    Ok
      { id = modul.id
      ; typ
      ; global
      ; table
      ; mem
      ; func
      ; elem
      ; data
      ; exports = modul.exports
      ; start = modul.start
      }
end

module Rewrite_indices = struct
  let find msg (named : 'a Named.t) (indice : Symbolic.indice option) :
    int Result.t =
    match indice with
    | None -> error_s "%s" msg
    | Some indice -> (
      match indice with
      | Symbolic.Arg.Raw i ->
        (* TODO change indexed strucure for that to be more efficient *)
        if not (List.exists (has_index i) named.values) then
          error_s "%s %i" msg i
        else Ok i
      | Symbolic name -> (
        match StringMap.find_opt name named.named with
        | None -> error_s "%s %s" msg name
        | Some i -> Ok i ) )

  let get msg (named : 'a Named.t) indice : 'a indexed Result.t =
    let* i = find msg named indice in
    (* TODO change Named.t structure to make that sensible *)
    match List.nth_opt named.values i with
    | None -> error_s "%s" msg
    | Some v -> Ok v

  let find_global (modul : assigned_module) ~imported_only id :
    (int * mut) Result.t =
    let* idx = find "unknown global" modul.global id in
    let va = List.find (has_index idx) modul.global.values in
    let* mut, _typ =
      match va.value with
      | Imported imported -> Ok imported.desc
      | Local global ->
        if imported_only then Error "unknown global"
        else
          let mut, val_type = global.typ in
          let val_type = convert_val_type val_type in
          Ok (mut, val_type)
    in
    Ok (idx, mut)

  let rewrite_expr (modul : assigned_module) (locals : param list)
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
        let t = convert_func_type t in
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
          let ok = equal_func_types t t' in
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
            if StringMap.mem name locals then error_s "duplicate local %s" name
            else Ok (StringMap.add name next_free_int locals, next_free_int + 1)
          )
        (Ok (StringMap.empty, 0))
        locals
    in

    let find_local = function
      | Symbolic.Arg.Raw i ->
        if i >= after_last_assigned_local then Error "unknown local" else Ok i
      | Symbolic name -> (
        match StringMap.find_opt name locals with
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
        | Some [ t ] -> ok @@ Select (Some [ convert_val_type t ])
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
      | Ref_null heap_type -> ok @@ Ref_null (convert_heap_type heap_type)
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
      | ( Array_new_data _ | Array_new _ | Array_new_elem _ | Array_new_fixed _
        | Array_get_u _ | Struct_get _ | Struct_get_s _ | Struct_set _
        | Struct_new _ | Struct_new_default _ | Ref_cast _ | Ref_test _
        | Br_on_non_null _ | Br_on_null _ | Br_on_cast _ | Br_on_cast_fail _ )
        as i ->
        Log.debug "TODO (Simplify.body) %a@\n" Symbolic.Pp.instr i;
        Ok Nop
    and expr (e : Symbolic.expr) (loop_count, block_ids) : expr Result.t =
      list_map (fun i -> body (loop_count, block_ids) i) e
    in
    expr iexpr (0, [])

  let rewrite_const_expr (modul : assigned_module) (expr : Symbolic.expr) :
    Const.expr Result.t =
    let const_instr (instr : Symbolic.instr) : Const.instr Result.t =
      let open Const in
      match instr with
      | Symbolic.I32_const v -> ok @@ I32_const v
      | I64_const v -> ok @@ I64_const v
      | F32_const v -> ok @@ F32_const v
      | F64_const v -> ok @@ F64_const v
      | Ref_null v -> ok @@ Ref_null (convert_heap_type v)
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

  let rewrite_block_type (modul : assigned_module)
    (block_type : Symbolic.block_type) : block_type Result.t =
    match block_type with
    | Symbolic.Arg.Bt_ind id ->
      let* t =
        match get "unknown type" modul.typ (Some id) with
        | Ok { value = Def_func_t t'; _ } -> Ok t'
        | Error _ as e -> e
        | Ok _ -> Error "TODO: Simplify.bt_some_to_raw"
      in
      Ok t
    | Bt_raw (_, func_type) -> Ok (convert_func_type func_type)

  let rewrite_global (modul : assigned_module) (global : Symbolic.global) :
    global Result.t =
    let* init = rewrite_const_expr modul global.init in
    let mut, val_type = global.typ in
    let typ = (mut, convert_val_type val_type) in
    let global = { id = global.id; init; typ } in
    Ok global

  let rewrite_elem (modul : assigned_module) (elem : Symbolic.elem) :
    elem Result.t =
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
    let typ = convert_ref_type elem.typ in
    let elem = { init; mode; id = elem.id; typ } in
    Ok elem

  let rewrite_data (modul : assigned_module) (data : Symbolic.data) :
    data Result.t =
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

  let rewrite_export msg named (exports : opt_export list) :
    export list Result.t =
    list_map
      (fun { name; id } ->
        let* id =
          match id with
          | Curr id -> Ok id
          | Indice id -> find msg named (Some id)
        in
        Ok ({ name; id } : export) )
      exports

  let rewrite_exports (modul : assigned_module) (exports : opt_exports) :
    exports Result.t =
    let* global = rewrite_export "unknown global" modul.global exports.global in
    let* mem = rewrite_export "unknown memory" modul.mem exports.mem in
    let* table = rewrite_export "unknown table" modul.table exports.table in
    let* func = rewrite_export "unknown function" modul.func exports.func in
    let e : exports = { global; mem; table; func } in
    Ok e

  let rewrite_func (modul : assigned_module) (func : Symbolic.func) :
    func Result.t =
    let* type_f = rewrite_block_type modul func.type_f in
    let params, _ = type_f in
    let locals = List.map convert_param func.locals in
    let* body = rewrite_expr modul (params @ locals) func.body in
    let func = { body; type_f; id = func.id; locals } in
    Ok func

  let rewrite_import (f : 'a -> 'b Result.t) (import : 'a imp) : 'b imp Result.t
      =
    let* desc = f import.desc in
    Ok { import with desc }

  let rewrite_runtime f g r =
    match r with
    | Local v ->
      let* v = f v in
      ok @@ Local v
    | Imported i ->
      let* i = g i in
      ok @@ Imported i

  let rewrite_named f named =
    let* values =
      list_map
        (fun ind ->
          let* value = f ind.value in
          Ok { ind with value } )
        named.Named.values
    in
    Ok { named with Named.values }

  let run (modul : assigned_module) : modul Result.t =
    let* (global : (global, global_type) runtime Named.t) =
      let* { Named.named; values } =
        rewrite_named (rewrite_runtime (rewrite_global modul) ok) modul.global
      in
      let values = List.rev values in
      let global : (global, global_type) runtime Named.t =
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
        let va = List.find (has_index idx) func.Named.values in
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
end

let modul (modul : Symbolic.modul) : modul Result.t =
  Log.debug "simplifying  ...@\n";
  let* (group : grouped_module) = group modul in
  let* (assigned : assigned_module) = Assign_indicies.run group in
  Rewrite_indices.run assigned
