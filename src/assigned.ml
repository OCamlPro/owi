open Syntax
open Types
open Simplified

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

type t =
  { id : string option
  ; typ : str_type Named.t
  ; global : (Symbolic.global, global_type) Runtime.t Named.t
  ; table : (table, table_type) Runtime.t Named.t
  ; mem : (mem, limits) Runtime.t Named.t
  ; func : (Symbolic.func, Symbolic.block_type) Runtime.t Named.t
  ; elem : Symbolic.elem Named.t
  ; data : Symbolic.data Named.t
  ; exports : Grouped.opt_exports
  ; start : Symbolic.indice option
  }

type type_acc =
  { declared_types : str_type Indexed.t list
  ; func_types : str_type Indexed.t list
  ; named_types : int String_map.t
  ; last_assigned_int : int
  ; all_types : int TypeMap.t
  }

let assign_types (modul : Grouped.t) : str_type Named.t =
  let assign_type
    { declared_types; func_types; named_types; last_assigned_int; all_types }
    (name, sub_type) : type_acc =
    let last_assigned_int, declared_types, named_types, all_types =
      match sub_type with
      | _final, _indices, str_type ->
        let str_type = Simplified_types.convert_str None str_type in
        let id = last_assigned_int in
        let last_assigned_int = succ last_assigned_int in
        let declared_types =
          { Indexed.index = id; value = str_type } :: declared_types
        in
        let named_types =
          match name with
          | None -> named_types
          | Some name -> String_map.add name id named_types
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
    ; named_types = String_map.empty
    ; last_assigned_int = 0
    ; all_types = TypeMap.empty
    }
  in
  let acc = List.fold_left assign_type empty_acc (List.rev modul.typ) in
  let assign_heap_type
    ({ func_types; named_types = _; last_assigned_int; all_types; _ } as acc)
    typ =
    let typ = Def_func_t (Simplified_types.convert_func_type None typ) in
    match TypeMap.find_opt typ all_types with
    | Some _id -> acc
    | None ->
      let id = last_assigned_int in
      let last_assigned_int = last_assigned_int + 1 in
      let all_types = TypeMap.add typ id all_types in
      let func_types =
        match typ with
        | Def_func_t _ftype -> { Indexed.index = id; value = typ } :: func_types
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

let get_runtime_name (get_name : 'a -> string option) (elt : ('a, 'b) Runtime.t)
  : string option =
  match elt with
  | Local v -> get_name v
  | Imported { assigned_name; _ } -> assigned_name

let name kind ~get_name values =
  let assign_one (named : int String_map.t) (elt : _ Indexed.t) =
    match get_name elt.value with
    | None -> Ok named
    | Some name ->
      if String_map.mem name named then error_s "duplicate %s %s" kind name
      else ok @@ String_map.add name elt.index named
  in
  let* named = list_fold_left assign_one String_map.empty values in
  Ok { Named.values; named }

let check_type_id (types : str_type Named.t) (check : Grouped.type_check) =
  let id, func_type = check in
  let* id =
    match id with
    | Raw i -> Ok i
    | Symbolic name -> (
      match String_map.find_opt name types.named with
      | None -> error_s "internal error: can't find type with name %s" name
      | Some t -> Ok t )
  in
  (* TODO more efficient version of that *)
  match List.find_opt (fun v -> v.Indexed.index = id) types.values with
  | None -> Error "unknown type"
  | Some { value = Def_func_t func_type'; _ } ->
    let func_type = Simplified_types.convert_func_type None func_type in
    if not (equal_func_types func_type func_type') then
      Error "inline function type"
    else Ok ()
  | Some _ -> Error "TODO: Simplify.check_type_id"

let of_grouped (modul : Grouped.t) : t Result.t =
  Log.debug "assigning    ...@\n";
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
