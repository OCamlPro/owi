(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Typetbl = Hashtbl.Make (struct
  type t = Text.func_type

  let equal = Text.func_type_eq

  let hash = Hashtbl.hash
end)

type t =
  { id : string option
  ; typ : Text.func_type Named.t
  ; global : (Text.Global.t, Text.Global.Type.t) Origin.t Named.t
  ; table : (Text.Table.t, Text.Table.Type.t) Origin.t Named.t
  ; mem : (Text.Mem.t, Text.limits) Origin.t Named.t
  ; func : (Text.Func.t, Text.block_type) Origin.t Named.t
  ; elem : Text.Elem.t Named.t
  ; data : Text.Data.t Named.t
  ; global_exports : Grouped.opt_export Dynarray.t
  ; mem_exports : Grouped.opt_export Dynarray.t
  ; table_exports : Grouped.opt_export Dynarray.t
  ; func_exports : Grouped.opt_export Dynarray.t
  ; start : Text.indice option
  }

let pp_id fmt id = Text.pp_id_opt fmt id

let pp_typ fmt typ = Named.pp Text.pp_func_type fmt typ

let pp_runtime_named ~pp_local ~pp_imported fmt l =
  Named.pp (Origin.pp ~pp_local ~pp_imported) fmt l

let pp_global fmt g =
  pp_runtime_named ~pp_local:Text.Global.pp ~pp_imported:Text.Global.Type.pp fmt
    g

let pp_table fmt t =
  pp_runtime_named ~pp_local:Text.Table.pp ~pp_imported:Text.Table.Type.pp fmt t

let pp_mem fmt m =
  pp_runtime_named ~pp_local:Text.Mem.pp ~pp_imported:Text.pp_limits fmt m

let pp_func fmt f =
  pp_runtime_named ~pp_local:Text.Func.pp ~pp_imported:Text.pp_block_type fmt f

let pp_elem fmt e = Named.pp Text.Elem.pp fmt e

let pp_data fmt d = Named.pp Text.Data.pp fmt d

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

let assign_types (modul : Grouped.t) : Text.func_type Named.t =
  let all_types = Typetbl.create 64 in
  let named_types = Hashtbl.create 64 in
  let declared_types = Dynarray.create () in
  Dynarray.iter
    (fun (name, typ) ->
      let id = Dynarray.length declared_types in
      begin match name with
      | None -> ()
      | Some name -> Hashtbl.add named_types name id
      end;
      Dynarray.add_last declared_types typ;
      Typetbl.add all_types typ id )
    modul.typ;
  Dynarray.iter
    (fun typ ->
      match Typetbl.find_opt all_types typ with
      | Some _id -> ()
      | None ->
        let id = Dynarray.length declared_types in
        Dynarray.add_last declared_types typ;
        Typetbl.add all_types typ id )
    modul.function_type;
  let declared_types = Dynarray.to_array declared_types in
  Named.create declared_types named_types

let get_runtime_name (get_name : 'a -> string option) (elt : ('a, 'b) Origin.t)
  : string option =
  match elt with
  | Local v -> get_name v
  | Imported { assigned_name; _ } -> assigned_name

let name kind ~get_name values =
  let named = Hashtbl.create 64 in
  let values = Dynarray.to_array values in
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
  Named.create values named

let check_type_id (types : Text.func_type Named.t) (id, func_type) =
  let id =
    match id with
    | Text.Raw i -> i
    | Text name -> (
      match Named.get_by_name types name with
      | None -> (* TODO: unchecked, is this actually reachable? *) assert false
      | Some v -> v )
  in
  match Named.get_at types id with
  | None -> Error (`Unknown_type (Text.Raw id))
  | Some func_type' ->
    if not (Text.func_type_eq func_type func_type') then
      Error `Inline_function_type
    else Ok ()

let of_grouped (modul : Grouped.t) : t Result.t =
  Log.debug (fun m -> m "assigning    ...");
  let typ = assign_types modul in
  let* global =
    name "global"
      ~get_name:(get_runtime_name (fun ({ id; _ } : Text.Global.t) -> id))
      modul.global
  in
  let* table =
    name "table"
      ~get_name:(get_runtime_name (fun ((id, _) : Text.Table.t) -> id))
      modul.table
  in
  let* mem =
    name "mem"
      ~get_name:(get_runtime_name (fun ((id, _) : Text.Mem.t) -> id))
      modul.mem
  in
  let* func =
    name "func"
      ~get_name:(get_runtime_name (fun ({ id; _ } : Text.Func.t) -> id))
      modul.func
  in
  let* elem =
    name "elem" ~get_name:(fun (elem : Text.Elem.t) -> elem.id) modul.elem
  in
  let* data =
    name "data" ~get_name:(fun (data : Text.Data.t) -> data.id) modul.data
  in
  let+ () = dynarray_iter (check_type_id typ) modul.type_checks in
  let modul =
    { id = modul.id
    ; typ
    ; global
    ; table
    ; mem
    ; func
    ; elem
    ; data
    ; global_exports = modul.global_exports
    ; mem_exports = modul.mem_exports
    ; table_exports = modul.table_exports
    ; func_exports = modul.func_exports
    ; start = modul.start
    }
  in
  Log.debug (fun m -> m "%a" pp modul);
  modul

let find (named : 'a Named.t) err : _ -> Binary.indice Result.t = function
  | Text.Raw i -> Ok i
  | Text name -> (
    match Named.get_by_name named name with None -> Error err | Some i -> Ok i )

let find_func modul id = find modul.func (`Unknown_func id) id

let find_global modul id = find modul.global (`Unknown_global id) id

let find_memory modul id = find modul.mem (`Unknown_memory id) id

let find_table modul id = find modul.table (`Unknown_table id) id

let find_data modul id = find modul.data (`Unknown_data id) id

let find_elem modul id = find modul.elem (`Unknown_elem id) id

let find_type modul id = find modul.typ (`Unknown_type id) id
