(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types
open Syntax

module StrType = struct
  type t = simplified str_type

  let compare = compare
end

module TypeMap = Map.Make (StrType)

let equal_func_types (a : simplified func_type) (b : simplified func_type) :
  bool =
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

type t =
  { id : string option
  ; typ : simplified str_type Named.t
  ; global : (Text.global, simplified global_type) Runtime.t Named.t
  ; table : (simplified table, simplified table_type) Runtime.t Named.t
  ; mem : (mem, limits) Runtime.t Named.t
  ; func : (text func, text block_type) Runtime.t Named.t
  ; elem : Text.elem Named.t
  ; data : Text.data Named.t
  ; exports : Grouped.opt_exports
  ; start : text indice option
  }

type type_acc =
  { declared_types : simplified str_type Indexed.t list
  ; func_types : simplified str_type Indexed.t list
  ; named_types : int String_map.t
  ; last_assigned_int : int
  ; all_types : int TypeMap.t
  }

let assign_type (acc : type_acc) (name, sub_type) : type_acc Result.t =
  let { declared_types; func_types; named_types; last_assigned_int; all_types }
      =
    acc
  in
  let+ last_assigned_int, declared_types, named_types, all_types =
    let _final, _indices, str_type = sub_type in
    let+ str_type = Simplified_types.convert_str None str_type in
    let id = last_assigned_int in
    let last_assigned_int = succ last_assigned_int in
    let declared_types = Indexed.return id str_type :: declared_types in
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

let assign_heap_type (acc : type_acc) typ : type_acc Result.t =
  let { func_types; last_assigned_int; all_types; _ } = acc in
  let+ typ = Simplified_types.convert_func_type None typ in
  let typ = Def_func_t typ in
  match TypeMap.find_opt typ all_types with
  | Some _id -> acc
  | None ->
    let id = last_assigned_int in
    let last_assigned_int = succ last_assigned_int in
    let all_types = TypeMap.add typ id all_types in
    let func_types =
      match typ with
      | Def_func_t _ftype -> Indexed.return id typ :: func_types
      | Def_array_t (_mut, _storage_type) -> func_types
      | Def_struct_t _ -> func_types
    in
    { acc with func_types; last_assigned_int; all_types }

let assign_types (modul : Grouped.t) : simplified str_type Named.t Result.t =
  let empty_acc : type_acc =
    { declared_types = []
    ; func_types = []
    ; named_types = String_map.empty
    ; last_assigned_int = 0
    ; all_types = TypeMap.empty
    }
  in
  let* acc = list_fold_left assign_type empty_acc (List.rev modul.typ) in
  let+ acc =
    list_fold_left assign_heap_type acc (List.rev modul.function_type)
  in
  let values = List.rev acc.declared_types @ List.rev acc.func_types in
  { Named.values; named = acc.named_types }

let get_runtime_name (get_name : 'a -> string option) (elt : ('a, 'b) Runtime.t)
  : string option =
  match elt with
  | Local v -> get_name v
  | Imported { assigned_name; _ } -> assigned_name

let name kind ~get_name values =
  let assign_one (named : int String_map.t) (elt : _ Indexed.t) =
    let elt_v = Indexed.get elt in
    match get_name elt_v with
    | None -> Ok named
    | Some name ->
      let index = Indexed.get_index elt in
      if String_map.mem name named then
        Error (`Msg (Format.sprintf "duplicate %s %s" kind name))
      else ok @@ String_map.add name index named
  in
  let+ named = list_fold_left assign_one String_map.empty values in
  { Named.values; named }

let check_type_id (types : simplified str_type Named.t)
  (check : Grouped.type_check) =
  let id, func_type = check in
  let id =
    match id with Raw i -> i | Text name -> String_map.find name types.named
  in
  (* TODO more efficient version of that *)
  match Indexed.get_at id types.values with
  | None -> Error `Unknown_type
  | Some (Def_func_t func_type') ->
    let* func_type = Simplified_types.convert_func_type None func_type in
    if not (equal_func_types func_type func_type') then
      Error `Inline_function_type
    else Ok ()
  | Some _ -> assert false

let of_grouped (modul : Grouped.t) : t Result.t =
  Log.debug0 "assigning    ...@\n";
  let* typ = assign_types modul in
  let* global =
    name "global"
      ~get_name:(get_runtime_name (fun ({ id; _ } : Text.global) -> id))
      modul.global
  in
  let* table =
    name "table"
      ~get_name:(get_runtime_name (fun ((id, _) : simplified table) -> id))
      modul.table
  in
  let* mem =
    name "mem"
      ~get_name:(get_runtime_name (fun ((id, _) : mem) -> id))
      modul.mem
  in
  let* func =
    name "func"
      ~get_name:(get_runtime_name (fun ({ id; _ } : text func) -> id))
      modul.func
  in
  let* elem =
    name "elem" ~get_name:(fun (elem : Text.elem) -> elem.id) modul.elem
  in
  let* data =
    name "data" ~get_name:(fun (data : Text.data) -> data.id) modul.data
  in
  let+ () = list_iter (check_type_id typ) modul.type_checks in
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
