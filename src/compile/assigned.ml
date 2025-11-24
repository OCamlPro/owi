(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

type t =
  { id : string option
  ; typ : Text.func_type Array.t
  ; global : (Text.Global.t, Text.Global.Type.t) Origin.t Array.t
  ; table : (Text.Table.t, Text.Table.Type.t) Origin.t Array.t
  ; mem : (Text.Mem.t, Text.limits) Origin.t Array.t
  ; func : (Text.Func.t, Text.block_type) Origin.t Array.t
  ; elem : Text.Elem.t Array.t
  ; data : Text.Data.t Array.t
  ; typ_names : (string, int) Hashtbl.t
  ; global_names : (string, int) Hashtbl.t
  ; table_names : (string, int) Hashtbl.t
  ; mem_names : (string, int) Hashtbl.t
  ; func_names : (string, int) Hashtbl.t
  ; elem_names : (string, int) Hashtbl.t
  ; data_names : (string, int) Hashtbl.t
  ; global_exports : Grouped.opt_export Array.t
  ; mem_exports : Grouped.opt_export Array.t
  ; table_exports : Grouped.opt_export Array.t
  ; func_exports : Grouped.opt_export Array.t
  ; start : Text.indice option
  }

let pp_id fmt id = Text.pp_id_opt fmt id

let pp_typ fmt typ = Fmt.array Text.pp_func_type fmt typ

let pp_global fmt g =
  Fmt.array
    (Origin.pp ~pp_local:Text.Global.pp ~pp_imported:Text.Global.Type.pp)
    fmt g

let pp_table fmt t =
  Fmt.array
    (Origin.pp ~pp_local:Text.Table.pp ~pp_imported:Text.Table.Type.pp)
    fmt t

let pp_mem fmt m =
  Fmt.array (Origin.pp ~pp_local:Text.Mem.pp ~pp_imported:Text.pp_limits) fmt m

let pp_func fmt f =
  Fmt.array
    (Origin.pp ~pp_local:Text.Func.pp ~pp_imported:Text.pp_block_type)
    fmt f

let pp_elem fmt e = Fmt.array Text.Elem.pp fmt e

let pp_data fmt d = Fmt.array Text.Data.pp fmt d

let pp_start fmt s = Text.pp_indice_opt fmt s

let pp fmt { id; typ; global; table; mem; func; elem; data; start; _ } =
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
     start: %a@\n\
     }"
    pp_id id pp_typ typ pp_global global pp_table table pp_mem mem pp_func func
    pp_elem elem pp_data data pp_start start

module Typetbl = Hashtbl.Make (struct
  type t = Text.func_type

  let equal = Text.func_type_eq

  let hash = Hashtbl.hash
end)

let assign_types typ function_type :
  Text.func_type Array.t * (string, int) Hashtbl.t =
  let all_types = Typetbl.create 64 in
  let named_types = Hashtbl.create 64 in
  let declared_types = Dynarray.create () in
  Array.iter
    (fun (name, typ) ->
      let id = Dynarray.length declared_types in
      begin match name with
      | None -> ()
      | Some name -> Hashtbl.add named_types name id
      end;
      Dynarray.add_last declared_types typ;
      Typetbl.add all_types typ id )
    typ;
  Array.iter
    (fun typ ->
      match Typetbl.find_opt all_types typ with
      | Some _id -> ()
      | None ->
        let id = Dynarray.length declared_types in
        Dynarray.add_last declared_types typ;
        Typetbl.add all_types typ id )
    function_type;
  let declared_types = Dynarray.to_array declared_types in
  (declared_types, named_types)

let get_origin_name (get_name : 'a -> string option) (elt : ('a, 'b) Origin.t) :
  string option =
  match elt with
  | Local v -> get_name v
  | Imported { assigned_name; _ } -> assigned_name

let name kind ~get_name values =
  let named = Hashtbl.create 64 in
  let+ () =
    array_iteri
      (fun i elt_v ->
        match get_name elt_v with
        | None -> Ok ()
        | Some name ->
          if Hashtbl.mem named name then
            Fmt.error_msg "duplicate %s %s" kind name
          else begin
            Hashtbl.add named name i;
            Ok ()
          end )
      values
  in
  named

let check_type_id (types : Text.func_type Array.t)
  (typ_names : (string, int) Hashtbl.t) (id, func_type) =
  let id =
    match id with
    | Text.Raw i -> i
    | Text name -> (
      match Hashtbl.find_opt typ_names name with
      | None -> assert false
      | Some v -> v )
  in
  if id >= Array.length types then Error (`Unknown_type (Text.Raw id))
  else
    let func_type' = Array.unsafe_get types id in
    if not (Text.func_type_eq func_type func_type') then
      Error `Inline_function_type
    else Ok ()

let of_grouped
  ({ global
   ; table
   ; mem
   ; func
   ; elem
   ; data
   ; type_checks
   ; id
   ; typ
   ; function_type
   ; global_exports
   ; mem_exports
   ; table_exports
   ; func_exports
   ; start
   } :
    Grouped.t ) : t Result.t =
  Log.debug (fun m -> m "assigning    ...");
  let typ, typ_names = assign_types typ function_type in
  let* global_names =
    name "global"
      ~get_name:(get_origin_name (fun ({ id; _ } : Text.Global.t) -> id))
      global
  in
  let* table_names =
    name "table"
      ~get_name:(get_origin_name (fun ((id, _) : Text.Table.t) -> id))
      table
  in
  let* mem_names =
    name "mem"
      ~get_name:(get_origin_name (fun ((id, _) : Text.Mem.t) -> id))
      mem
  in
  let* func_names =
    name "func"
      ~get_name:(get_origin_name (fun ({ id; _ } : Text.Func.t) -> id))
      func
  in
  let* elem_names =
    name "elem" ~get_name:(fun (elem : Text.Elem.t) -> elem.id) elem
  in
  let* data_names =
    name "data" ~get_name:(fun (data : Text.Data.t) -> data.id) data
  in
  let+ () = array_iter (check_type_id typ typ_names) type_checks in
  let modul =
    { id
    ; typ
    ; global
    ; table
    ; mem
    ; func
    ; elem
    ; data
    ; typ_names
    ; global_names
    ; table_names
    ; mem_names
    ; func_names
    ; elem_names
    ; data_names
    ; global_exports
    ; mem_exports
    ; table_exports
    ; func_exports
    ; start
    }
  in
  Log.debug (fun m -> m "%a" pp modul);
  modul

let find (names : (string, int) Hashtbl.t) err : _ -> Binary.indice Result.t =
  function
  | Text.Raw i -> Ok i
  | Text name -> (
    match Hashtbl.find_opt names name with None -> Error err | Some i -> Ok i )

let find_func modul id = find modul.func_names (`Unknown_func id) id

let find_global modul id = find modul.global_names (`Unknown_global id) id

let find_memory modul id = find modul.mem_names (`Unknown_memory id) id

let find_table modul id = find modul.table_names (`Unknown_table id) id

let find_data modul id = find modul.data_names (`Unknown_data id) id

let find_elem modul id = find modul.elem_names (`Unknown_elem id) id

let find_type modul id = find modul.typ_names (`Unknown_type id) id
