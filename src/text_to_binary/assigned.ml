(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax

module Type = struct
  type t = func_type

  let compare = Types.compare_func_type
end

module TypeMap = Map.Make (Type)

type t =
  { id : string option
  ; typ : Type.t Named.t
  ; global : (Text.global, global_type) Runtime.t Named.t
  ; table : (table, table_type) Runtime.t Named.t
  ; mem : (mem, limits) Runtime.t Named.t
  ; func : (text func, text block_type) Runtime.t Named.t
  ; elem : Text.elem Named.t
  ; data : Text.data Named.t
  ; exports : Grouped.opt_exports
  ; start : text indice option
  ; annots : text Annot.annot list
  }

let sep fmt () = Fmt.pf fmt " ; "

let pp_id fmt id = Types.pp_id_opt fmt id

let pp_typ fmt typ = Named.pp Types.pp_func_type fmt typ

let pp_runtime_named ~pp_local ~pp_imported fmt l =
  Named.pp (Runtime.pp ~pp_local ~pp_imported) fmt l

let pp_global fmt g =
  pp_runtime_named ~pp_local:Text.pp_global ~pp_imported:Types.pp_global_type
    fmt g

let pp_table fmt t =
  pp_runtime_named ~pp_local:Types.pp_table ~pp_imported:Types.pp_table_type fmt
    t

let pp_mem fmt m =
  pp_runtime_named ~pp_local:Types.pp_mem ~pp_imported:Types.pp_limits fmt m

let pp_func fmt f =
  pp_runtime_named ~pp_local:Types.pp_func ~pp_imported:Types.pp_block_type fmt
    f

let pp_elem fmt e = Named.pp Text.pp_elem fmt e

let pp_data fmt d = Named.pp Text.pp_data fmt d

let pp_start fmt s = Types.pp_indice_opt fmt s

let pp_annots fmt annots =
  Fmt.pf fmt "[%a]" (Fmt.list ~sep Annot.pp_annot) annots

let pp fmt
  { id; typ; global; table; mem; func; elem; data; exports; start; annots } =
  Fmt.pf fmt
    "{@\n\
    \  @[<v>id: %a@\n\
     typ: %a@\n\
     global: %a@\n\
     table: %a@\n\
     mem: %a@\n\
     func: %a@\n\
     elem: %a@\n\
     data: %a@\n\
     exports: %a@\n\
     start: %a@\n\
     annots: %a@]@\n\
     }"
    pp_id id pp_typ typ pp_global global pp_table table pp_mem mem pp_func func
    pp_elem elem pp_data data Grouped.pp_opt_exports exports pp_start start
    pp_annots annots

type type_acc =
  { declared_types : func_type Indexed.t list
  ; func_types : func_type Indexed.t list
  ; named_types : int String_map.t
  ; last_assigned_int : int
  ; all_types : int TypeMap.t
  }

let assign_type (acc : type_acc) (name, func_type) : type_acc =
  let { declared_types; func_types; named_types; last_assigned_int; all_types }
      =
    acc
  in
  let last_assigned_int, declared_types, named_types, all_types =
    let id = last_assigned_int in
    let last_assigned_int = succ last_assigned_int in
    let declared_types = Indexed.return id func_type :: declared_types in
    let named_types =
      match name with
      | None -> named_types
      | Some name -> String_map.add name id named_types
    in
    let all_types = TypeMap.add func_type id all_types in
    (last_assigned_int, declared_types, named_types, all_types)
  in

  (* Is there something to do/check when a type is already declared ? *)
  { declared_types; func_types; named_types; last_assigned_int; all_types }

let assign_heap_type (acc : type_acc) typ : type_acc =
  let { func_types; last_assigned_int; all_types; _ } = acc in
  match TypeMap.find_opt typ all_types with
  | Some _id -> acc
  | None ->
    let id = last_assigned_int in
    let last_assigned_int = succ last_assigned_int in
    let all_types = TypeMap.add typ id all_types in
    let func_types = Indexed.return id typ :: func_types in
    { acc with func_types; last_assigned_int; all_types }

let assign_types (modul : Grouped.t) : func_type Named.t =
  let empty_acc : type_acc =
    { declared_types = []
    ; func_types = []
    ; named_types = String_map.empty
    ; last_assigned_int = 0
    ; all_types = TypeMap.empty
    }
  in
  let acc = List.fold_left assign_type empty_acc (List.rev modul.typ) in
  let acc =
    List.fold_left assign_heap_type acc (List.rev modul.function_type)
  in
  let values = List.rev acc.declared_types @ List.rev acc.func_types in
  Named.create values acc.named_types

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
        Fmt.error_msg "duplicate %s %s" kind name
      else ok @@ String_map.add name index named
  in
  let+ named = list_fold_left assign_one String_map.empty values in
  Named.create values named

let check_type_id (types : func_type Named.t)
  ((id, func_type) : Grouped.type_check) =
  let id =
    match id with
    | Raw i -> i
    | Text name -> (
      match String_map.find_opt name types.named with
      | None -> (* TODO: unchecked, is this actually reachable? *) assert false
      | Some v -> v )
  in
  (* TODO more efficient version of that *)
  match Indexed.get_at id types.values with
  | None -> Error (`Unknown_type (Raw id))
  | Some func_type' ->
    if not (Types.func_type_eq func_type func_type') then
      Error `Inline_function_type
    else Ok ()

let of_grouped (modul : Grouped.t) : t Result.t =
  Log.debug (fun m -> m "assigning    ...");
  let typ = assign_types modul in
  let* global =
    name "global"
      ~get_name:(get_runtime_name (fun ({ id; _ } : Text.global) -> id))
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
  let modul =
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
    ; annots = modul.annots
    }
  in
  Log.debug (fun m -> m "%a" pp modul);
  modul

let find (named : 'a Named.t) err : _ -> binary indice Result.t = function
  | Raw _i as indice -> Ok indice
  | Text name -> (
    match String_map.find_opt name named.named with
    | None -> Error err
    | Some i -> Ok (Raw i) )

let find_func modul id = find modul.func (`Unknown_func id) id

let find_global modul id = find modul.global (`Unknown_global id) id

let find_memory modul id = find modul.mem (`Unknown_memory id) id

let find_table modul id = find modul.table (`Unknown_table id) id

let find_data modul id = find modul.data (`Unknown_data id) id

let find_elem modul id = find modul.elem (`Unknown_elem id) id

let find_type modul id = find modul.typ (`Unknown_type id) id
