open Types

type function_import = indice block_type

type table_import = table_type

type mem_import = mem_type

type global_import = global_type

type 'a imp =
  { module_ : string
  ; name : string
  ; assigned_name : string option
  ; desc : 'a
  }

type ('a, 'b) runtime =
  | Local of 'a
  | Imported of 'b imp

type type_check = indice * func_type

type grouped_module =
  { id : string option
  ; type_ : type_ list
  ; function_type : func_type list
        (* Types comming from function declarations.
           It contains potential duplication *)
  ; type_checks : type_check list
        (* Types checks to perform after assignment.
           Come from function declarations with type indicies *)
  ; global : (global, global_import) runtime list
  ; table : (table, table_import) runtime list
  ; mem : (mem, mem_import) runtime list
  ; func : (indice func, function_import) runtime list
  ; elem : elem list
  ; data : data list
  ; export : export list
  ; start : indice list
  }

module StringMap = Map.Make (String)

module Group : sig
  val group : module_ -> grouped_module
end = struct
  let imp (import : import) (assigned_name, desc) =
    { module_ = import.module_; name = import.name; assigned_name; desc }

  let empty_module id =
    { id
    ; type_ = []
    ; function_type = []
    ; type_checks = []
    ; global = []
    ; table = []
    ; mem = []
    ; func = []
    ; elem = []
    ; data = []
    ; export = []
    ; start = []
    }

  let group (module_ : Types.module_) : grouped_module =
    let add field fields =
      match field with
      | MType type_ -> { fields with type_ = type_ :: fields.type_ }
      | MGlobal global -> { fields with global = Local global :: fields.global }
      | MImport ({ desc = Import_global (a, b); _ } as import) ->
        { fields with global = Imported (imp import (a, b)) :: fields.global }
      | MTable table -> { fields with table = Local table :: fields.table }
      | MImport ({ desc = Import_table (a, b); _ } as import) ->
        { fields with table = Imported (imp import (a, b)) :: fields.table }
      | MMem mem -> { fields with mem = Local mem :: fields.mem }
      | MImport ({ desc = Import_mem (a, b); _ } as import) ->
        { fields with mem = Imported (imp import (a, b)) :: fields.mem }
      | MFunc func ->
        let function_type, type_checks =
          match func.type_f with
          | Bt_ind _ -> (fields.function_type, fields.type_checks)
          | Bt_raw (id, type_) ->
            let type_checks =
              match id with
              | None -> fields.type_checks
              | Some id -> (id, type_) :: fields.type_checks
            in
            (type_ :: fields.function_type, type_checks)
        in
        let func = Local func :: fields.func in
        { fields with func; function_type; type_checks }
      | MImport ({ desc = Import_func (a, b); _ } as import) ->
        { fields with func = Imported (imp import (a, b)) :: fields.func }
      | MElem elem -> { fields with elem = elem :: fields.elem }
      | MData data -> { fields with data = data :: fields.data }
      | MExport export -> { fields with export = export :: fields.export }
      | MStart start -> { fields with start = start :: fields.start }
    in
    List.fold_right add module_.fields (empty_module module_.id)
end

type index = simplified_indice

type 'a indexed =
  { index : index
  ; value : 'a
  }

type 'a named =
  { values : 'a indexed list
  ; named : index StringMap.t
  }

module Fields = struct
  type 'a t = 'a named

  let fold f v acc =
    List.fold_left (fun acc v -> f v.index v.value acc) acc v.values
end

type assigned_module =
  { id : string option
  ; type_ : func_type named
  ; global :
      ( (indice, (indice, indice block_type) expr') global'
      , global_import )
      runtime
      named
  ; table : (table, table_import) runtime named
  ; mem : (mem, mem_import) runtime named
  ; func : ((indice, indice block_type) func', indice block_type) runtime named
  ; elem : (indice, (indice, indice block_type) expr') elem' named
  ; data : (indice, (indice, indice block_type) expr') data' named
  ; export : indice export' list
  ; start : indice list
  }

type result =
  { id : string option
  ; global : ((index, Const.expr) global', global_import) runtime named
  ; table : (table, table_import) runtime named
  ; mem : (mem, mem_import) runtime named
  ; func : ((index, func_type) func', func_type) runtime named
  ; elem : (index, Const.expr) elem' named
  ; data : (index, Const.expr) data' named
  ; export : index export' list
  ; start : index list
  }

module FuncType = struct
  type t = func_type

  let compare = compare
end

module TypeMap = Map.Make (FuncType)

let equal_func_types (a : func_type) (b : func_type) : bool =
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

module Assign_indicies : sig
  val run : grouped_module -> assigned_module
end = struct
  type type_acc =
    { declared_types : func_type indexed list
    ; named_types : index StringMap.t
    ; last_assigned_index : int
    ; all_types : index TypeMap.t
    }

  let assign_types (module_ : grouped_module) : func_type named =
    let assign_type
        { declared_types; named_types; last_assigned_index; all_types }
        (name, type_) =
      let id = I last_assigned_index in
      let last_assigned_index = last_assigned_index + 1 in
      let declared_types = { index = id; value = type_ } :: declared_types in
      let named_types =
        match name with
        | None -> named_types
        | Some name -> StringMap.add name id named_types
      in
      let all_types = TypeMap.add type_ id all_types in
      (* Is there something to do/check when a type is already declared ? *)
      { declared_types; named_types; last_assigned_index; all_types }
    in
    let empty_acc =
      { declared_types = []
      ; named_types = StringMap.empty
      ; last_assigned_index = 0
      ; all_types = TypeMap.empty
      }
    in
    let acc = List.fold_left assign_type empty_acc module_.type_ in
    let assign_func_type
        ( { declared_types; named_types = _; last_assigned_index; all_types } as
        acc ) type_ =
      match TypeMap.find_opt type_ all_types with
      | Some _id -> acc
      | None ->
        let id = I last_assigned_index in
        let last_assigned_index = last_assigned_index + 1 in
        let declared_types = { index = id; value = type_ } :: declared_types in
        let all_types = TypeMap.add type_ id all_types in
        { acc with declared_types; last_assigned_index; all_types }
    in
    let acc = List.fold_left assign_func_type acc module_.function_type in
    { values = List.rev acc.declared_types; named = acc.named_types }

  let assign ~(get_name : 'a -> string option) (values : 'a list) : 'a named =
    let assign_one (declared, named, last_assigned_index) elt =
      let id = I last_assigned_index in
      let last_assigned_index = last_assigned_index + 1 in
      let named =
        match get_name elt with
        | None -> named
        | Some name -> StringMap.add name id named
      in
      (declared, named, last_assigned_index)
    in
    let declared, named, _last_assigned_index =
      List.fold_left assign_one ([], StringMap.empty, 0) values
    in
    { values = declared; named }

  let get_runtime_name (get_name : 'a -> string option) (elt : ('a, 'b) runtime)
      : string option =
    match elt with
    | Local v -> get_name v
    | Imported { assigned_name; _ } -> assigned_name

  let check_type_id (types : func_type named) (check : type_check) =
    let id, func_type = check in
    let id =
      match id with
      | Raw i -> I i
      | Symbolic name -> StringMap.find name types.named
    in
    (* TODO more efficient version of that *)
    match List.find_opt (fun v -> v.index = id) types.values with
    | None -> failwith "Unbound type"
    | Some func_type' ->
      if not (equal_func_types func_type func_type'.value) then
        failwith "inline func type"

  let run (module_ : grouped_module) : assigned_module =
    let type_ = assign_types module_ in
    let global =
      assign
        ~get_name:(get_runtime_name (fun ({ id; _ } : Types.global) -> id))
        module_.global
    in
    let table =
      assign
        ~get_name:(get_runtime_name (fun ((id, _) : Types.table) -> id))
        module_.table
    in
    let mem =
      assign
        ~get_name:(get_runtime_name (fun ((id, _) : Types.mem) -> id))
        module_.mem
    in
    let func =
      assign
        ~get_name:(get_runtime_name (fun ({ id; _ } : _ Types.func) -> id))
        module_.func
    in
    let elem =
      assign ~get_name:(fun (elem : Types.elem) -> elem.id) module_.elem
    in
    let data =
      assign ~get_name:(fun (data : Types.data) -> data.id) module_.data
    in
    List.iter (check_type_id type_) module_.type_checks;
    { id = module_.id
    ; type_
    ; global
    ; table
    ; mem
    ; func
    ; elem
    ; data
    ; export = module_.export
    ; start = module_.start
    }
end

module Rewrite_indices = struct
  let find msg (named : 'a named) (indice : indice) : index =
    match indice with
    | Raw i -> I i
    | Symbolic name -> (
      match StringMap.find_opt name named.named with
      | None -> failwith msg
      | Some i -> i )

  let get msg (named : 'a named) (indice : indice) : 'a indexed =
    let (I i) = find msg named indice in
    (* TODO change named structure to make that sensible *)
    match List.nth_opt named.values i with None -> failwith msg | Some v -> v

  let rewrite_expr (module_ : assigned_module) (locals : param list)
      (iexpr : expr) : (index, func_type) expr' =
    (* block_ids handling *)
    let block_id_to_raw (loop_count, block_ids) id : index =
      let id =
        match id with
        | Symbolic id ->
          Debug.debug Format.err_formatter "SYMBOLIC BLOCK ID %s@\n" id;
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
          if !pos = -1 then failwith "unknown label";
          !pos
        | Raw id ->
          Debug.debug Format.err_formatter "RAW BLOCK ID %d@\n" id;
          id
      in
      (* this is > and not >= because you can `br 0` without any block to target the function *)
      if id > List.length block_ids + loop_count then failwith "unknown label";
      I id
    in

    let bt_to_raw =
      Option.map (function
        | Bt_ind ind ->
          let { value; _ } = get "unknown type" module_.type_ ind in
          value
        | Bt_raw (type_use, t) ->
          begin
            match type_use with
            | None -> ()
            | Some ind ->
              (* TODO: move this to check ? *)
              (* we check that the explicit type match the type_use, we have to remove parameters names to do so *)
              let { value = t'; _ } = get "unknown type" module_.type_ ind in
              let ok = equal_func_types t t' in
              if not ok then failwith "inline function type"
          end;
          t )
    in

    let find_local =
      let locals, last_assigned_local =
        List.fold_left
          (fun (locals, next_free_index) ((name, _type) : param) ->
            match name with
            | None -> (locals, next_free_index + 1)
            | Some name ->
              ( StringMap.add name (I next_free_index) locals
              , next_free_index + 1 ) )
          (StringMap.empty, 0) locals
      in
      let find_local id =
        match id with
        | Raw i ->
          if i > last_assigned_local then failwith "unknown local";
          I i
        | Symbolic name -> (
          match StringMap.find_opt name locals with
          | None -> failwith "unknown local"
          | Some id -> id )
      in
      find_local
    in

    let find_table id = find "unknown table" module_.table id in
    let find_func id = find "unknown function" module_.func id in
    let find_global id = find "unknown global" module_.global id in
    let _find_mem id = find "unknown memory" module_.mem id in
    let find_data id = find "unknown data segment" module_.data id in
    let find_elem id = find "unknown elem segment" module_.elem id in

    let rec body (loop_count, block_ids) : instr -> (index, func_type) instr' =
      function
      | Br_table (ids, id) ->
        let f = block_id_to_raw (loop_count, block_ids) in
        Br_table (Array.map f ids, f id)
      | Br_if id -> Br_if (block_id_to_raw (loop_count, block_ids) id)
      | Br id -> Br (block_id_to_raw (loop_count, block_ids) id)
      | Call id -> Call (find_func id)
      | Local_set id -> Local_set (find_local id)
      | Local_get id -> Local_get (find_local id)
      | Local_tee id -> Local_tee (find_local id)
      | If_else (id, bt, e1, e2) ->
        let bt = bt_to_raw bt in
        let block_ids = id :: block_ids in
        If_else
          ( id
          , bt
          , expr e1 (loop_count, block_ids)
          , expr e2 (loop_count, block_ids) )
      | Loop (id, bt, e) ->
        let bt = bt_to_raw bt in
        let e = expr e (loop_count + 1, id :: block_ids) in
        Loop (id, bt, e)
      | Block (id, bt, e) ->
        let bt = bt_to_raw bt in
        Block (id, bt, expr e (loop_count, id :: block_ids))
      | Call_indirect (tbl_i, bt) ->
        let bt = Option.get @@ bt_to_raw (Some bt) in
        Call_indirect (find_table tbl_i, bt)
      | Global_set id ->
        (* TODO: move this to check, this is not doable without linking *)
        (*
        (* TODO: it seems there's not test for this, add one ? *)
        begin
          match global with
          | Local ((mut, _vt), _) | Imported (_, _, (mut, _vt)) -> begin
            match mut with Const -> failwith "global is immutable" | Var -> ()
          end
        end;*)
        Global_set (find_global id)
      | Global_get id -> Global_get (find_global id)
      | Ref_func id -> Ref_func (find_func id)
      | Table_size id -> Table_size (find_table id)
      | Table_get id -> Table_get (find_table id)
      | Table_set id -> Table_set (find_table id)
      | Table_grow id -> Table_grow (find_table id)
      | Table_init (i, i') -> Table_init (find_table i, find_elem i')
      | Table_fill id -> Table_fill (find_table id)
      | Table_copy (i, i') -> Table_copy (find_table i, find_table i')
      | Memory_init id ->
        if List.length module_.mem.values < 1 then failwith "unknown memory";
        Memory_init (find_data id)
      | Data_drop id -> Data_drop (find_data id)
      | Elem_drop id -> Elem_drop (find_elem id)
      | ( I_load8 _ | I_load16 _ | I64_load32 _ | I_load _ | F_load _
        | I64_store32 _ | I_store8 _ | I_store16 _ | F_store _ | I_store _
        | Memory_copy | Memory_size | Memory_fill | Memory_grow ) as i ->
        if List.length module_.mem.values < 1 then failwith "unknown memory";
        i
      | ( I_unop _ | I_binop _ | I_testop _ | I_relop _ | F_unop _ | F_relop _
        | I32_wrap_i64 | Ref_null _ | F_reinterpret_i _ | I_reinterpret_f _
        | I64_extend_i32 _ | I64_extend32_s | F32_demote_f64 | I_extend8_s _
        | I_extend16_s _ | F64_promote_f32 | F_convert_i _ | I_trunc_f _
        | I_trunc_sat_f _ | Ref_is_null | F_binop _ | F32_const _ | F64_const _
        | I32_const _ | I64_const _ | Unreachable | Drop | Select _ | Nop
        | Return ) as i ->
        i
    and expr (e : expr) (loop_count, block_ids) : (index, func_type) expr' =
      List.map (body (loop_count, block_ids)) e
    in
    let body = expr iexpr (0, []) in
    body

  let rewrite_const_expr (module_ : assigned_module) (expr : (indice, _) expr')
      : Const.expr =
    let const_ibinop (op : ibinop) : Const.ibinop =
      match op with
      | Add -> Add
      | Sub -> Sub
      | Mul -> Mul
      | _ -> failwith "not a constant expression"
    in

    let const_instr (instr : (indice, _) instr') : Const.instr =
      match instr with
      | I32_const v -> I32_const v
      | I64_const v -> I64_const v
      | F32_const v -> F32_const v
      | F64_const v -> F64_const v
      | Ref_null v -> Ref_null v
      | Global_get id -> Global_get (find "unknown global" module_.global id)
      | I_binop (t, op) -> I_binop (t, const_ibinop op)
      | _ -> failwith "not a constant expression"
    in
    List.map const_instr expr

  let rewrite_block_type (module_ : assigned_module) block_type : func_type =
    match block_type with
    | Bt_ind id -> (get "unbound type" module_.type_ id).value
    | Bt_raw (_, func_type) -> func_type

  let rewrite_global (module_ : assigned_module) (global : global) :
      (index, Const.expr) global' =
    { global with init = rewrite_const_expr module_ global.init }

  let rewrite_elem (module_ : assigned_module) (elem : elem) :
      (index, Const.expr) elem' =
    let mode =
      match elem.mode with
      | (Elem_passive | Elem_declarative) as mode -> mode
      | Elem_active (indice_opt, expr) ->
        let indice_opt =
          Option.map (find "unbound elem" module_.elem) indice_opt
        in
        let expr = rewrite_const_expr module_ expr in
        Elem_active (indice_opt, expr)
    in
    let init = List.map (rewrite_const_expr module_) elem.init in
    { elem with init; mode }

  let rewrite_data (module_ : assigned_module) (data : data) :
      (index, Const.expr) data' =
    let mode =
      match data.mode with
      | Data_passive as mode -> mode
      | Data_active (indice_opt, expr) ->
        let indice_opt =
          Option.map (find "unbound data" module_.data) indice_opt
        in
        let expr = rewrite_const_expr module_ expr in
        Data_active (indice_opt, expr)
    in
    { data with mode }

  let rewrite_export (module_ : assigned_module) (export : indice export') :
      index export' =
    let desc =
      match export.desc with
      | Export_func id ->
        Export_func ((Option.map (find "unbound func" module_.func)) id)
      | Export_table id ->
        Export_table ((Option.map (find "unbound table" module_.table)) id)
      | Export_mem id ->
        Export_mem ((Option.map (find "unbound mem" module_.mem)) id)
      | Export_global id ->
        Export_global ((Option.map (find "unbound global" module_.global)) id)
    in
    { export with desc }

  let rewrite_func (module_ : assigned_module) (func : indice func) :
      (index, func_type) func' =
    let body = rewrite_expr module_ func.locals func.body in
    let type_f = rewrite_block_type module_ func.type_f in
    { func with body; type_f }

  let rewrite_import (f : 'a -> 'b) (import : 'a imp) : 'b imp =
    { import with desc = f import.desc }

  let rewrite_runtime f g r =
    match r with Local v -> Local (f v) | Imported i -> Imported (g i)

  let rewrite_named f named =
    { named with
      values =
        List.map (fun ind -> { ind with value = f ind.value }) named.values
    }

  let run (module_ : assigned_module) : result =
    let global =
      rewrite_named
        (rewrite_runtime (rewrite_global module_) Fun.id)
        module_.global
    in
    let elem = rewrite_named (rewrite_elem module_) module_.elem in
    let data = rewrite_named (rewrite_data module_) module_.data in
    let export = List.map (rewrite_export module_) module_.export in
    let func =
      rewrite_named
        (rewrite_runtime (rewrite_func module_)
           (rewrite_import (rewrite_block_type module_)) )
        module_.func
    in
    let start = List.map (find "unbound func" module_.func) module_.start in
    { id = module_.id
    ; mem = module_.mem
    ; table = module_.table
    ; global
    ; elem
    ; data
    ; export
    ; func
    ; start
    }
end

let simplify (module_ : module_) : result =
  Group.group module_ |> Assign_indicies.run |> Rewrite_indices.run