open Types
open Syntax
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

type export =
  { name : string
  ; id : int
  }

type opt_ind =
  | Curr of int
  | Indice of indice

type opt_export =
  { name : string
  ; id : opt_ind
  }

type type_check = indice * str_type

type 'a indexed =
  { index : int
  ; value : 'a
  }

let has_index idx { index; _ } = idx = index

module Named = struct
  type 'a t =
    { values : 'a indexed list
    ; named : int StringMap.t
    }

  let fold f v acc =
    List.fold_left (fun acc v -> f v.index v.value acc) acc v.values

  let iter f v = List.iter (fun v -> f v.index v.value) v.values
end

type exports =
  { global : export list
  ; mem : export list
  ; table : export list
  ; func : export list
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

type known_elem = (indice, (indice, indice block_type) expr') elem'

type known_data = (indice, (indice, indice block_type) expr') data'

type grouped_module =
  { id : string option
  ; type_ : type_def list
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
  ; exports : opt_exports
  ; start : indice option
  }

type assigned_module =
  { id : string option
  ; type_ : str_type Named.t
  ; global : (global, global_import) runtime Named.t
  ; table : (table, table_import) runtime Named.t
  ; mem : (mem, mem_import) runtime Named.t
  ; func : (indice func, indice block_type) runtime Named.t
  ; elem : known_elem Named.t
  ; data : known_data Named.t
  ; exports : opt_exports
  ; start : indice option
  }

type simplified_module =
  { id : string option
  ; global : (Const.expr global', global_import) runtime Named.t
  ; table : (table, table_import) runtime Named.t
  ; mem : (mem, mem_import) runtime Named.t
  ; func : ((int, func_type) func', func_type) runtime Named.t
  ; elem : (int, Const.expr) elem' Named.t
  ; data : (int, Const.expr) data' Named.t
  ; exports : exports
  ; start : int option
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

  let global fmt (func : (_ global', _) runtime) =
    match func with
    | Local func -> begin
      match func.id with
      | None -> Format.fprintf fmt "local"
      | Some id -> Format.fprintf fmt "$%s" id
    end
    | Imported { module_; name; _ } -> Format.fprintf fmt "%s.%s" module_ name

  let indexed f fmt indexed =
    Format.fprintf fmt "%i: %a" indexed.index f indexed.value

  let lst f fmt l =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") f)
      l

  let funcs fmt (funcs : _ runtime Named.t) =
    lst (indexed func) fmt funcs.values

  let globals fmt (globals : _ runtime Named.t) =
    lst (indexed global) fmt globals.values

  let export fmt (export : export) =
    Format.fprintf fmt "%s: %a" export.name Pp.Simplified.indice export.id

  let simplified_module fmt (m : simplified_module) : unit =
    fprintf fmt
      "@[<hov 2>(simplified_module%a@ @[<hov 2>(func %a)@]@ @[<hov 2>(global \
       %a)@]@ @[<hov 2>(export func %a)@]@@ )@]"
      id m.id funcs m.func globals m.global (lst export) m.exports.func
end

module Group : sig
  val group : module_ -> (grouped_module, string) Result.t
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

  let init_curr =
    { global = 0; table = 0; mem = 0; func = 0; elem = 0; data = 0 }

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

  let curr_id curr = function None -> Curr (pred curr) | Some id -> Indice id

  let check_limit { min; max } =
    match max with
    | None -> Ok ()
    | Some max ->
      if min > max then Error "size minimum must not be greater than maximum"
      else Ok ()

  let group (module_ : Types.module_) =
    let add ((fields : grouped_module), curr) field =
      match field with
      | MType type_ -> ok ({ fields with type_ = type_ @ fields.type_ }, curr)
      | MGlobal global -> ok @@ add_global (Local global) fields curr
      | MImport ({ desc = Import_global (a, b); _ } as import) ->
        ok @@ add_global (Imported (imp import (a, b))) fields curr
      | MExport { name; desc = Export_global id } ->
        let id = curr_id curr.global id in
        let exports =
          { fields.exports with global = { name; id } :: fields.exports.global }
        in
        ok ({ fields with exports }, curr)
      | MTable table ->
        let _, (limits, _) = table in
        let* () = check_limit limits in
        ok @@ add_table (Local table) fields curr
      | MImport ({ desc = Import_table (a, b); _ } as import) ->
        ok @@ add_table (Imported (imp import (a, b))) fields curr
      | MExport { name; desc = Export_table id } ->
        let id = curr_id curr.table id in
        let exports =
          { fields.exports with table = { name; id } :: fields.exports.table }
        in
        ok ({ fields with exports }, curr)
      | MMem mem ->
        let _, limits = mem in
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
      | MImport ({ desc = Import_mem (a, b); _ } as import) ->
        ok @@ add_mem (Imported (imp import (a, b))) fields curr
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
          | Bt_raw (id, type_) ->
            let type_checks =
              match id with
              | None -> fields.type_checks
              | Some id -> (id, Def_func_t type_) :: fields.type_checks
            in
            (type_ :: fields.function_type, type_checks)
        in
        let index = curr.func in
        let func = { value = Local func; index } :: fields.func in
        Ok
          ( { fields with func; function_type; type_checks }
          , { curr with func = succ curr.func } )
      | MImport ({ desc = Import_func (a, b); _ } as import) ->
        ok @@ add_func (Imported (imp import (a, b))) fields curr
      | MExport { name; desc = Export_func id } ->
        let id = curr_id curr.func id in
        let exports =
          { fields.exports with func = { name; id } :: fields.exports.func }
        in
        Ok ({ fields with exports }, curr)
      | MElem elem ->
        let mode =
          match elem.mode with
          | (Elem_passive | Elem_declarative) as mode -> mode
          | Elem_active (id, expr) ->
            let id = Option.value id ~default:(Raw (curr.table - 1)) in
            Elem_active (id, expr)
        in
        ok @@ add_elem { elem with mode } fields curr
      | MData data ->
        let mode =
          match data.mode with
          | Data_passive -> Data_passive
          | Data_active (id, expr) ->
            let id = Option.value id ~default:(Raw (curr.mem - 1)) in
            Data_active (id, expr)
        in
        ok @@ add_data { data with mode } fields curr
      | MStart start -> Ok ({ fields with start = Some start }, curr)
    in
    let* module_, _curr =
      list_fold_left add (empty_module module_.id, init_curr) module_.fields
    in
    Ok module_
end

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

module Assign_indicies : sig
  val run : grouped_module -> (assigned_module, string) Result.t
end = struct
  type type_acc =
    { declared_types : str_type indexed list
    ; func_types : str_type indexed list
    ; named_types : int StringMap.t
    ; last_assigned_int : int
    ; all_types : int TypeMap.t
    }

  let assign_types (module_ : grouped_module) : str_type Named.t =
    let assign_type
      { declared_types; func_types; named_types; last_assigned_int; all_types }
      (name, sub_type) : type_acc =
      let last_assigned_int, declared_types, named_types, all_types =
        match sub_type with
        | ((_final, _indices, str_type) : sub_type) ->
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
    let acc = List.fold_left assign_type empty_acc (List.rev module_.type_) in
    let assign_func_type
      ({ func_types; named_types = _; last_assigned_int; all_types; _ } as acc)
      type_ =
      match type_ with
      | Def_func_t _ftype -> begin
        match TypeMap.find_opt type_ all_types with
        | Some _id -> acc
        | None ->
          let id = last_assigned_int in
          let last_assigned_int = last_assigned_int + 1 in
          let func_types = { index = id; value = type_ } :: func_types in
          let all_types = TypeMap.add type_ id all_types in
          { acc with func_types; last_assigned_int; all_types }
      end
      | Def_array_t _ | Def_struct_t _ -> acc
    in
    let acc =
      List.fold_left assign_func_type acc
        (List.rev_map (fun ftype -> Def_func_t ftype) module_.function_type)
    in
    let values = List.rev acc.declared_types @ List.rev acc.func_types in
    (* Format.printf "TYPES@.%a"
     *   (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@.")
     *      (fun ppf {int; value} -> Format.fprintf ppf "%a: %a"
     *                       Pp.Simplified_bis.indice int Pp.Input.func_type value))
     *   values; *)
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
    let* func_type =
      match func_type with
      | Def_array_t _ | Def_struct_t _ -> Error "TODO: Simplify.check_type_id"
      | Def_func_t func_type -> Ok func_type
    in
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
      if not (equal_func_types func_type func_type') then
        Error "inline function type"
      else Ok ()
    | Some _ -> Error "TODO: Simplify.check_type_id"

  let run (module_ : grouped_module) : (assigned_module, string) Result.t =
    let type_ = assign_types module_ in
    let* global =
      name "global"
        ~get_name:(get_runtime_name (fun ({ id; _ } : Types.global) -> id))
        module_.global
    in
    let* table =
      name "table"
        ~get_name:(get_runtime_name (fun ((id, _) : Types.table) -> id))
        module_.table
    in
    let* mem =
      name "mem"
        ~get_name:(get_runtime_name (fun ((id, _) : Types.mem) -> id))
        module_.mem
    in
    let* func =
      name "func"
        ~get_name:(get_runtime_name (fun ({ id; _ } : _ Types.func) -> id))
        module_.func
    in
    let* elem =
      name "elem" ~get_name:(fun (elem : known_elem) -> elem.id) module_.elem
    in
    let* data =
      name "data" ~get_name:(fun (data : known_data) -> data.id) module_.data
    in
    let* () = list_iter (check_type_id type_) module_.type_checks in
    Ok
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
  let find msg (named : 'a Named.t) indice : (int, string) Result.t =
    match indice with
    | Raw i ->
      (* TODO change indexed strucure for that to be more efficient *)
      if not (List.exists (has_index i) named.values) then error_s "%s %i" msg i
      else Ok i
    | Symbolic name -> (
      match StringMap.find_opt name named.named with
      | None -> error_s "%s %s" msg name
      | Some i -> Ok i )

  let get msg (named : 'a Named.t) indice : ('a indexed, string) Result.t =
    let* i = find msg named indice in
    (* TODO change Named.t structure to make that sensible *)
    match List.nth_opt named.values i with
    | None -> error_s "%s" msg
    | Some v -> Ok v

  let find_global (module_ : assigned_module) ~imported_only id =
    let* idx = find "unknown global" module_.global id in
    let va = List.find (has_index idx) module_.global.values in
    let* mut, _typ =
      match va.value with
      | Local global ->
        if imported_only then Error "unknown global" else Ok global.type_
      | Imported imported -> Ok imported.desc
    in
    Ok (idx, mut)

  let rewrite_expr (module_ : assigned_module) (locals : param list)
    (iexpr : expr) : ((int, func_type) expr', string) Result.t =
    (* block_ids handling *)
    let block_id_to_raw (loop_count, block_ids) id =
      let* id =
        match id with
        | Symbolic id ->
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

    let bt_some_to_raw = function
      | Bt_ind ind -> begin
        match get "unknown type" module_.type_ ind with
        | Ok { value = Def_func_t t'; _ } -> Ok t'
        | Error _ as e -> e
        | Ok _ -> Error "TODO: Simplify.bt_some_to_raw"
      end
      | Bt_raw (type_use, t) -> (
        match type_use with
        | None -> Ok t
        | Some ind ->
          (* we check that the explicit type match the type_use, we have to remove parameters names to do so *)
          let* t' =
            match get "unknown type" module_.type_ ind with
            | Ok { value = Def_func_t t'; _ } -> Ok t'
            | Error _ as e -> e
            | Ok _ -> Error "TODO: Simplify.bt_some_to_raw"
          in
          let ok = equal_func_types t t' in
          if not ok then Error "inline function type" else Ok t )
    in

    let bt_to_raw = function
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
      | Raw i ->
        if i >= after_last_assigned_local then Error "unknown local" else Ok i
      | Symbolic name -> (
        match StringMap.find_opt name locals with
        | None -> error_s "unknown local %s" name
        | Some id -> Ok id )
    in

    let find_table id = find "unknown table" module_.table id in
    let find_func id = find "unknown function" module_.func id in
    let _find_mem id = find "unknown memory" module_.mem id in
    let find_data id = find "unknown data segment" module_.data id in
    let find_elem id = find "unknown elem segment" module_.elem id in

    let rec body (loop_count, block_ids) :
      instr -> ((int, func_type) instr', string) Result.t = function
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
        let* id = find_func id in
        ok @@ Call id
      | Return_call id ->
        let* id = find_func id in
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
        let* tbl_i = find_table tbl_i in
        let* bt = bt_some_to_raw bt in
        ok @@ Call_indirect (tbl_i, bt)
      | Return_call_indirect (tbl_i, bt) ->
        let* tbl_i = find_table tbl_i in
        let* bt = bt_some_to_raw bt in
        ok @@ Return_call_indirect (tbl_i, bt)
      | Global_set id -> begin
        let* idx, mut = find_global module_ ~imported_only:false id in
        match mut with
        | Const -> Error "global is immutable"
        | Var -> ok @@ Global_set idx
      end
      | Global_get id ->
        let* idx, _mut = find_global module_ ~imported_only:false id in
        ok @@ Global_get idx
      | Ref_func id ->
        let* id = find_func id in
        ok @@ Ref_func id
      | Table_size id ->
        let* id = find_table id in
        ok @@ Table_size id
      | Table_get id ->
        let* id = find_table id in
        ok @@ Table_get id
      | Table_set id ->
        let* id = find_table id in
        ok @@ Table_set id
      | Table_grow id ->
        let* id = find_table id in
        ok @@ Table_grow id
      | Table_init (i, i') ->
        let* table = find_table i in
        let* elem = find_elem i' in
        ok @@ Table_init (table, elem)
      | Table_fill id ->
        let* id = find_table id in
        ok @@ Table_fill id
      | Table_copy (i, i') ->
        let* table = find_table i in
        let* table' = find_table i' in
        ok @@ Table_copy (table, table')
      | Memory_init id ->
        if List.length module_.mem.values < 1 then Error "unknown memory 0"
        else
          let* id = find_data id in
          ok @@ Memory_init id
      | Data_drop id ->
        let* id = find_data id in
        ok @@ Data_drop id
      | Elem_drop id ->
        let* id = find_elem id in
        ok @@ Elem_drop id
      (* TODO: should we check alignment or memory existence first ? is it tested in the reference implementation ? *)
      | (I_load8 (_, _, { align; _ }) | I_store8 (_, { align; _ })) as i ->
        if List.length module_.mem.values < 1 then Error "unknown memory 0"
        else if align >= 1 then
          Error "alignment must not be larger than natural"
        else Ok i
      | (I_load16 (_, _, { align; _ }) | I_store16 (_, { align; _ })) as i ->
        if List.length module_.mem.values < 1 then Error "unknown memory 0"
        else if align >= 2 then
          Error "alignment must not be larger than natural"
        else Ok i
      | (I64_load32 (_, { align; _ }) | I64_store32 { align; _ }) as i ->
        if List.length module_.mem.values < 1 then Error "unknown memory 0"
        else if align >= 4 then
          Error "alignment must not be larger than natural"
        else Ok i
      | ( I_load (nn, { align; _ })
        | F_load (nn, { align; _ })
        | F_store (nn, { align; _ })
        | I_store (nn, { align; _ }) ) as i ->
        if List.length module_.mem.values < 1 then Error "unknown memory 0"
        else
          let max_allowed = match nn with S32 -> 4 | S64 -> 8 in
          if align >= max_allowed then
            Error "alignment must not be larger than natural"
          else Ok i
      | (Memory_copy | Memory_size | Memory_fill | Memory_grow) as i ->
        if List.length module_.mem.values < 1 then Error "unknown memory 0"
        else Ok i
      | Select typ as i -> begin
        match typ with
        | None | Some [ _ ] -> Ok i
        | Some [] | Some (_ :: _ :: _) -> Error "invalid result arity"
      end
      | ( I_unop _ | I_binop _ | I_testop _ | I_relop _ | F_unop _ | F_relop _
        | I32_wrap_i64 | Ref_null _ | F_reinterpret_i _ | I_reinterpret_f _
        | I64_extend_i32 _ | I64_extend32_s | F32_demote_f64 | I_extend8_s _
        | I_extend16_s _ | F64_promote_f32 | F_convert_i _ | I_trunc_f _
        | I_trunc_sat_f _ | Ref_is_null | F_binop _ | F32_const _ | F64_const _
        | I32_const _ | I64_const _ | Unreachable | Drop | Nop | Return ) as i
        ->
        Ok i
    and expr (e : expr) (loop_count, block_ids) :
      ((int, func_type) expr', string) Result.t =
      list_map (fun i -> body (loop_count, block_ids) i) e
    in
    expr iexpr (0, [])

  let rewrite_const_expr (module_ : assigned_module) (expr : (indice, _) expr')
    : (Const.expr, string) Result.t =
    let const_instr (instr : (indice, _) instr') :
      (Const.instr, string) Result.t =
      match instr with
      | I32_const v -> ok @@ Const.I32_const v
      | I64_const v -> ok @@ Const.I64_const v
      | F32_const v -> ok @@ Const.F32_const v
      | F64_const v -> ok @@ Const.F64_const v
      | Ref_null v -> ok @@ Const.Ref_null v
      | Ref_func f ->
        let* f = find "unknown function" module_.func f in
        ok @@ Const.Ref_func f
      | Global_get id -> begin
        let* idx, mut = find_global module_ ~imported_only:true id in
        match mut with
        | Const -> ok @@ Const.Global_get idx
        | Var -> Error "constant expression required"
      end
      | _ -> Error "constant expression required"
    in
    list_map const_instr expr

  let rewrite_block_type (module_ : assigned_module) block_type =
    match block_type with
    | Bt_ind id ->
      let* t =
        match get "unknown type" module_.type_ id with
        | Ok { value = Def_func_t t'; _ } -> Ok t'
        | Error _ as e -> e
        | Ok _ -> Error "TODO: Simplify.bt_some_to_raw"
      in
      Ok t
    | Bt_raw (_, func_type) -> Ok func_type

  let rewrite_global (module_ : assigned_module) (global : global) :
    (Const.expr global', string) Result.t =
    let* init = rewrite_const_expr module_ global.init in
    Ok { global with init }

  let rewrite_elem (module_ : assigned_module) (elem : known_elem) =
    let* mode =
      match elem.mode with
      | (Elem_passive | Elem_declarative) as mode -> Ok mode
      | Elem_active (indice, expr) ->
        let* indice = find "unknown table" module_.table indice in
        let* expr = rewrite_const_expr module_ expr in
        ok @@ Elem_active (indice, expr)
    in
    let* init = list_map (rewrite_const_expr module_) elem.init in
    Ok { elem with init; mode }

  let rewrite_data (module_ : assigned_module) (data : known_data) =
    let* mode =
      match data.mode with
      | Data_passive as mode -> Ok mode
      | Data_active (indice, expr) ->
        let* indice = find "unknown memory" module_.mem indice in
        let* expr = rewrite_const_expr module_ expr in
        ok @@ Data_active (indice, expr)
    in
    Ok { data with mode }

  let rewrite_export msg named (exports : opt_export list) :
    (export list, string) Result.t =
    list_map
      (fun { name; id } ->
        let* id =
          match id with Curr id -> Ok id | Indice id -> find msg named id
        in
        Ok ({ name; id } : export) )
      exports

  let rewrite_exports (module_ : assigned_module) (exports : opt_exports) :
    (exports, string) Result.t =
    let* global =
      rewrite_export "unknown global" module_.global exports.global
    in
    let* mem = rewrite_export "unknown memory" module_.mem exports.mem in
    let* table = rewrite_export "unknown table" module_.table exports.table in
    let* func = rewrite_export "unknown function" module_.func exports.func in
    let e : exports = { global; mem; table; func } in
    Ok e

  let rewrite_func (module_ : assigned_module) (func : indice func) :
    ((int, func_type) func', string) Result.t =
    let* type_f = rewrite_block_type module_ func.type_f in
    let params, _ = type_f in
    let* body = rewrite_expr module_ (params @ func.locals) func.body in
    Ok { func with body; type_f }

  let rewrite_import (f : 'a -> ('b, string) Result.t) (import : 'a imp) :
    ('b imp, string) Result.t =
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

  let run (module_ : assigned_module) : (simplified_module, string) Result.t =
    let* global =
      let* { Named.named; values } =
        rewrite_named
          (rewrite_runtime (rewrite_global module_) ok)
          module_.global
      in
      Ok { Named.named; values = List.rev values }
    in
    let* elem = rewrite_named (rewrite_elem module_) module_.elem in
    let* data = rewrite_named (rewrite_data module_) module_.data in
    let* exports = rewrite_exports module_ module_.exports in
    let* func =
      let import = rewrite_import (rewrite_block_type module_) in
      let runtime = rewrite_runtime (rewrite_func module_) import in
      rewrite_named runtime module_.func
    in
    let* start =
      match module_.start with
      | None -> Ok None
      | Some start -> (
        let* idx = find "unknown function" func start in
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
    Ok
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

let module_ (module_ : module_) =
  Log.debug "simplifying  ...@\n";
  let* group = Group.group module_ in
  let* assigned = Assign_indicies.run group in
  Rewrite_indices.run assigned

let pp = P.simplified_module
