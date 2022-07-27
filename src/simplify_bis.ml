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
  { elements : 'a indexed list
  ; named : index StringMap.t
  }

type assigned_module =
  { id : string option
  ; type_ : func_type named
  ; global : (global, global_import) runtime named
  ; table : (table, table_import) runtime named
  ; mem : (mem, mem_import) runtime named
  ; func : (indice func, function_import) runtime named
  ; elem : elem named
  ; data : data named
  ; export : export list
  ; start : indice list
  }

module Assign_indicies : sig
  val run : grouped_module -> assigned_module
end = struct
  module FuncType = struct
    type t = func_type

    let compare = compare
  end

  module TypeMap = Map.Make (FuncType)

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
    { elements = List.rev acc.declared_types; named = acc.named_types }

  let assign ~(get_name : 'a -> string option) (elements : 'a list) : 'a named =
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
      List.fold_left assign_one ([], StringMap.empty, 0) elements
    in
    { elements = declared; named }

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
    match List.find_opt (fun v -> v.index = id) types.elements with
    | None -> failwith "Unbound type"
    | Some func_type' ->
      if not (func_type = func_type'.value) then failwith "BAD type"

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

module Rewrite_indices = struct end
