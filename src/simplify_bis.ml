open Types
module StringMap = Map.Make (String)

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

type 'a export =
  { name : string
  ; id : 'a
  }

type type_check = indice * func_type

type index = simplified_indice

type opt_ind =
  | Curr of index
  | Indice of indice

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
    List.fold_left (fun acc v -> f v.index v.value acc) acc (List.rev v.values)
end

type 'a exports =
  { global : 'a export list
  ; mem : 'a export list
  ; table : 'a export list
  ; func : 'a export list
  }

type known_elem = (indice, (indice, indice block_type) expr') elem'

type known_data = (indice, (indice, indice block_type) expr') data'

type grouped_module =
  { id : string option
  ; type_ : type_ list
  ; function_type : func_type list
        (* Types comming from function declarations.
           It contains potential duplication *)
  ; type_checks : type_check list
        (* Types checks to perform after assignment.
           Come from function declarations with type indicies *)
  ; global : (global, global_import) runtime indexed list
  ; table : (table, table_import) runtime indexed list
  ; mem : (mem, mem_import) runtime indexed list
  ; func : (indice func, function_import) runtime indexed list
  ; elem : known_elem indexed list
  ; data : known_data indexed list
  ; exports : opt_ind exports
  ; start : indice list
  }

type assigned_module =
  { id : string option
  ; type_ : func_type named
  ; global : (global, global_import) runtime named
  ; table : (table, table_import) runtime named
  ; mem : (mem, mem_import) runtime named
  ; func : (indice func, indice block_type) runtime named
  ; elem : known_elem named
  ; data : known_data named
  ; exports : opt_ind exports
  ; start : indice list
  }

type result =
  { id : string option
  ; global : ((Const.expr) global', global_import) runtime named
  ; table : (table, table_import) runtime named
  ; mem : (mem, mem_import) runtime named
  ; func : ((index, func_type) func', func_type) runtime named
  ; elem : (index, Const.expr) elem' named
  ; data : (index, Const.expr) data' named
  ; exports : index exports
  ; start : index list
  }

module P = struct
  open Format

  let id fmt = Option.iter (fun id -> Format.fprintf fmt "@ %s" id)

  let func fmt (func : (_ func', _) runtime) =
    match func with
    | Local func -> begin
      match func.id with
      | None -> Format.fprintf fmt "local"
      | Some id -> Format.fprintf fmt "$%s" id
    end
    | Imported { module_; name; _ } -> Format.fprintf fmt "%s.%s" module_ name

  let indexed f fmt indexed =
    let (I i) = indexed.index in
    Format.fprintf fmt "%i: %a" i f indexed.value

  let lst f fmt l =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") f)
      l

  let funcs fmt (funcs : _ runtime named) = lst (indexed func) fmt funcs.values

  let export fmt (export : index export) =
    Format.fprintf fmt "%s: %a" export.name Pp.Simplified_bis.indice export.id

  let result fmt (result : result) : unit =
    fprintf fmt
      "@[<hov 2>(simplified_module%a@ @[<hov 2>(func %a)@]@ @[<hov 2>(export \
       func %a)@]@@ )@]"
      id result.id funcs result.func (lst export) result.exports.func
end

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
    ; exports = { global = []; table = []; mem = []; func = [] }
    ; start = []
    }

  type curr =
    { global : int
    ; table : int
    ; mem : int
    ; func : int
    ; elem : int
    ; data : int
    }

  let init_curr =
    { global = 0; table = 0; mem = 0; func = 0; elem = 0; data = 0 }

  let add_global value (fields : grouped_module) (curr : curr) =
    let index = I curr.global in
    ( { fields with global = { index; value } :: fields.global }
    , { curr with global = succ curr.global } )

  let add_table value (fields : grouped_module) (curr : curr) =
    let index = I curr.table in
    ( { fields with table = { index; value } :: fields.table }
    , { curr with table = succ curr.table } )

  let add_mem value (fields : grouped_module) (curr : curr) =
    let index = I curr.mem in
    ( { fields with mem = { index; value } :: fields.mem }
    , { curr with mem = succ curr.mem } )

  let add_func value (fields : grouped_module) (curr : curr) =
    let index = I curr.func in
    ( { fields with func = { index; value } :: fields.func }
    , { curr with func = succ curr.func } )

  let add_elem value (fields : grouped_module) (curr : curr) =
    let index = I curr.elem in
    ( { fields with elem = { index; value } :: fields.elem }
    , { curr with elem = succ curr.elem } )

  let add_data value (fields : grouped_module) (curr : curr) =
    let index = I curr.data in
    ( { fields with data = { index; value } :: fields.data }
    , { curr with data = succ curr.data } )

  let curr_id curr = function
    | None -> Curr (I (pred curr))
    | Some id -> Indice id

  let group (module_ : Types.module_) : grouped_module =
    let add ((fields : grouped_module), curr) field =
      match field with
      | MType type_ -> ({ fields with type_ = type_ :: fields.type_ }, curr)
      | MGlobal global -> add_global (Local global) fields curr
      | MImport ({ desc = Import_global (a, b); _ } as import) ->
        add_global (Imported (imp import (a, b))) fields curr
      | MExport { name; desc = Export_global id } ->
        let id = curr_id curr.global id in
        ( { fields with
            exports =
              { fields.exports with
                global = { name; id } :: fields.exports.global
              }
          }
        , curr )
      | MTable table -> add_table (Local table) fields curr
      | MImport ({ desc = Import_table (a, b); _ } as import) ->
        add_table (Imported (imp import (a, b))) fields curr
      | MExport { name; desc = Export_table id } ->
        let id = curr_id curr.table id in
        ( { fields with
            exports =
              { fields.exports with
                table = { name; id } :: fields.exports.table
              }
          }
        , curr )
      | MMem mem -> add_mem (Local mem) fields curr
      | MImport ({ desc = Import_mem (a, b); _ } as import) ->
        add_mem (Imported (imp import (a, b))) fields curr
      | MExport { name; desc = Export_mem id } ->
        let id = curr_id curr.mem id in
        ( { fields with
            exports =
              { fields.exports with mem = { name; id } :: fields.exports.mem }
          }
        , curr )
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
        let index = I curr.func in
        let func = { value = Local func; index } :: fields.func in
        ( { fields with func; function_type; type_checks }
        , { curr with func = succ curr.func } )
      | MImport ({ desc = Import_func (a, b); _ } as import) ->
        add_func (Imported (imp import (a, b))) fields curr
      | MExport { name; desc = Export_func id } ->
        let id = curr_id curr.func id in
        ( { fields with
            exports =
              { fields.exports with func = { name; id } :: fields.exports.func }
          }
        , curr )
      | MElem elem ->
        let mode =
          match elem.mode with
          | (Elem_passive | Elem_declarative) as mode -> mode
          | Elem_active (id, expr) ->
            let id = Option.value id ~default:(Raw (curr.table - 1)) in
            Elem_active (id, expr)
        in
        let elem = { elem with mode } in
        add_elem elem fields curr
      | MData data ->
        let mode =
          match data.mode with
          | Data_passive -> Data_passive
          | Data_active (id, expr) ->
            let id = Option.value id ~default:(Raw (curr.mem - 1)) in
            Data_active (id, expr)
        in
        let data = { data with mode } in

        add_data data fields curr
      | MStart start -> ({ fields with start = start :: fields.start }, curr)
    in
    let module_, _curr =
      List.fold_left add (empty_module module_.id, init_curr) module_.fields
    in
    module_
end

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
    ; func_types : func_type indexed list
    ; named_types : index StringMap.t
    ; last_assigned_index : int
    ; all_types : index TypeMap.t
    }

  let assign_types (module_ : grouped_module) : func_type named =
    let assign_type
        { declared_types
        ; func_types
        ; named_types
        ; last_assigned_index
        ; all_types
        } (name, type_) =
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
      { declared_types
      ; func_types
      ; named_types
      ; last_assigned_index
      ; all_types
      }
    in
    let empty_acc =
      { declared_types = []
      ; func_types = []
      ; named_types = StringMap.empty
      ; last_assigned_index = 0
      ; all_types = TypeMap.empty
      }
    in
    let acc = List.fold_left assign_type empty_acc (List.rev module_.type_) in
    let assign_func_type
        ( { func_types; named_types = _; last_assigned_index; all_types; _ } as
        acc ) type_ =
      match TypeMap.find_opt type_ all_types with
      | Some _id -> acc
      | None ->
        let id = I last_assigned_index in
        let last_assigned_index = last_assigned_index + 1 in
        let func_types = { index = id; value = type_ } :: func_types in
        let all_types = TypeMap.add type_ id all_types in
        { acc with func_types; last_assigned_index; all_types }
    in
    let acc =
      List.fold_left assign_func_type acc (List.rev module_.function_type)
    in
    let values = List.rev acc.declared_types @ List.rev acc.func_types in
    (* Format.printf "TYPES@.%a"
     *   (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@.")
     *      (fun ppf {index; value} -> Format.fprintf ppf "%a: %a"
     *                       Pp.Simplified_bis.indice index Pp.Input.func_type value))
     *   values; *)
    { values; named = acc.named_types }

  let get_runtime_name (get_name : 'a -> string option) (elt : ('a, 'b) runtime)
      : string option =
    match elt with
    | Local v -> get_name v
    | Imported { assigned_name; _ } -> assigned_name

  let name ~(get_name : 'a -> string option) (values : 'a indexed list) :
      'a named =
    let assign_one (named : index StringMap.t) (elt : _ indexed) =
      match get_name elt.value with
      | None -> named
      | Some name -> StringMap.add name elt.index named
    in
    let named = List.fold_left assign_one StringMap.empty values in
    { values; named }

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
      name
        ~get_name:(get_runtime_name (fun ({ id; _ } : Types.global) -> id))
        module_.global
    in
    let table =
      name
        ~get_name:(get_runtime_name (fun ((id, _) : Types.table) -> id))
        module_.table
    in
    let mem =
      name
        ~get_name:(get_runtime_name (fun ((id, _) : Types.mem) -> id))
        module_.mem
    in
    let func =
      name
        ~get_name:(get_runtime_name (fun ({ id; _ } : _ Types.func) -> id))
        module_.func
    in
    let elem =
      name ~get_name:(fun (elem : known_elem) -> elem.id) module_.elem
    in
    let data =
      name ~get_name:(fun (data : known_data) -> data.id) module_.data
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
    ; exports = module_.exports
    ; start = module_.start
    }
end

module Rewrite_indices = struct
  let find msg (named : 'a named) (indice : indice) : index =
    match indice with
    | Raw i ->
      (* TODO change indexed strucure for that to be more efficient *)
      if not (List.exists (fun { index; _ } -> index = I i) named.values) then
        failwith msg;
      I i
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
          | None -> failwith (Printf.sprintf "unknown local %s" name)
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
      | _ -> failwith "constant expression required"
    in

    let const_instr (instr : (indice, _) instr') : Const.instr =
      match instr with
      | I32_const v -> I32_const v
      | I64_const v -> I64_const v
      | F32_const v -> F32_const v
      | F64_const v -> F64_const v
      | Ref_null v -> Ref_null v
      | Ref_func f -> Ref_func (find "unknown func" module_.func f)
      | Global_get id -> Global_get (find "unknown global" module_.global id)
      | I_binop (t, op) -> I_binop (t, const_ibinop op)
      | _ -> failwith "constant expression required"
    in
    List.map const_instr expr

  let rewrite_block_type (module_ : assigned_module) block_type : func_type =
    match block_type with
    | Bt_ind id -> (get "unknown type" module_.type_ id).value
    | Bt_raw (_, func_type) -> func_type

  let rewrite_global (module_ : assigned_module) (global : global) :
      (Const.expr) global' =
    { global with init = rewrite_const_expr module_ global.init }

  let rewrite_elem (module_ : assigned_module) (elem : known_elem) :
      (index, Const.expr) elem' =
    let mode =
      match elem.mode with
      | (Elem_passive | Elem_declarative) as mode -> mode
      | Elem_active (indice, expr) ->
        let indice = find "unknown table" module_.table indice in
        let expr = rewrite_const_expr module_ expr in
        Elem_active (indice, expr)
    in
    let init = List.map (rewrite_const_expr module_) elem.init in
    { elem with init; mode }

  let rewrite_data (module_ : assigned_module) (data : known_data) :
      (index, Const.expr) data' =
    let mode =
      match data.mode with
      | Data_passive as mode -> mode
      | Data_active (indice, expr) ->
        let indice = find "unknown memory" module_.mem indice in
        let expr = rewrite_const_expr module_ expr in
        Data_active (indice, expr)
    in
    { data with mode }

  let rewrite_export msg named exports =
    List.map
      (fun { name; id } ->
        let id =
          match id with Curr id -> id | Indice id -> find msg named id
        in
        { name; id } )
      exports

  let rewrite_exports (module_ : assigned_module) (exports : opt_ind exports) :
      index exports =
    { global = rewrite_export "unknown global" module_.global exports.global
    ; mem = rewrite_export "unknown mem" module_.mem exports.mem
    ; table = rewrite_export "unknown table" module_.table exports.table
    ; func = rewrite_export "unknown func" module_.func exports.func
    }

  let rewrite_func (module_ : assigned_module) (func : indice func) :
      (index, func_type) func' =
    let type_f = rewrite_block_type module_ func.type_f in
    let params, _ = type_f in
    let body = rewrite_expr module_ (params @ func.locals) func.body in
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
    let exports = rewrite_exports module_ module_.exports in
    let func =
      rewrite_named
        (rewrite_runtime (rewrite_func module_)
           (rewrite_import (rewrite_block_type module_)) )
        module_.func
    in
    let start = List.map (find "unknown func" module_.func) module_.start in
    { id = module_.id
    ; mem = module_.mem
    ; table = module_.table
    ; global
    ; elem
    ; data
    ; exports
    ; func
    ; start
    }
end

type func = (index, func_type) func'

let simplify (module_ : module_) : result =
  Group.group module_ |> Assign_indicies.run |> Rewrite_indices.run

let simplify (module_ : module_) : result =
  let simplified = simplify module_ in
  Debug.debugerr "@ @[<hov 2>SIMPLIFIED:@\n%a@]@ " P.result simplified;
  simplified

module Pp = P
