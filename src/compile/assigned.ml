(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

type t =
  { typ : Text.sub_type Array.t
  ; typ_names : (string, int) Hashtbl.t
  ; field_names : (int, (string, int) Hashtbl.t) Hashtbl.t
  ; global_names : (string, int) Hashtbl.t
  ; table_names : (string, int) Hashtbl.t
  ; mem_names : (string, int) Hashtbl.t
  ; func_names : (string, int) Hashtbl.t
  ; elem_names : (string, int) Hashtbl.t
  ; data_names : (string, int) Hashtbl.t
  ; tag_names : (string, int) Hashtbl.t
  }

let pp_table fmt tbl =
  Fmt.iter_bindings
    ~sep:(fun fmt () -> Fmt.pf fmt " ; ")
    Hashtbl.iter
    (fun fmt (name, n) -> Fmt.pf fmt "(%S, %d)" name n)
    fmt tbl

let pp fmt
  { typ
  ; typ_names
  ; field_names = _
  ; global_names
  ; table_names
  ; mem_names
  ; func_names
  ; elem_names
  ; data_names
  ; tag_names
  } =
  Fmt.pf fmt
    "Types: %a@\n\
     Types names: %a@\n\
     Global names: %a@\n\
     Table names: %a@\n\
     Mem names: %a@\n\
     Func names: %a@\n\
     Elem names: %a@\n\
     Data names: %a@\n\
     Tag names: %a@\n"
    (Fmt.array Text.pp_sub_type)
    typ pp_table typ_names pp_table global_names pp_table table_names pp_table
    mem_names pp_table func_names pp_table elem_names pp_table data_names
    pp_table tag_names

module Typetbl = Hashtbl.Make (struct
  type t = Text.func_type

  let equal = Text.func_type_eq

  let hash = Hashtbl.hash
end)

let add_sub_type ~name ~sub_type ~declared_types ~named_types ~field_names =
  let id = Dynarray.length declared_types in
  Option.iter (fun n -> Hashtbl.add named_types n id) name;
  match sub_type with
  | Text.{ ct = Def_struct_t fields; _ } ->
    let tbl = Hashtbl.create 16 in
    let* _idx =
      list_fold_left
        (fun idx (field_id, _) ->
          let* () =
            match field_id with
            | Some (Text.Text n) when Hashtbl.mem tbl n ->
              Error (`Msg "duplicate field")
            | Some (Text.Text n) ->
              Hashtbl.add tbl n idx;
              Ok ()
            | Some (Text.Raw _) | None -> Ok ()
          in
          Ok (idx + 1) )
        0 fields
    in
    if Hashtbl.length tbl > 0 then Hashtbl.add field_names id tbl;
    Dynarray.add_last declared_types sub_type;
    Ok ()
  | _ ->
    Dynarray.add_last declared_types sub_type;
    Ok ()

let assign_types (typ : Text.Typedef.t array) decl_types :
  ( Text.sub_type Array.t
  * (string, int) Hashtbl.t
  * (int, (string, int) Hashtbl.t) Hashtbl.t )
  Result.t =
  let all_func_types = Typetbl.create 64 in
  let named_types = Hashtbl.create 64 in
  let field_names : (int, (string, int) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 16
  in
  let declared_types : Text.sub_type Dynarray.t = Dynarray.create () in
  let* () =
    array_iter
      (fun typedef ->
        match typedef with
        | Text.Typedef.SimpleType
            ( name
            , ( { Text.final = true; ids = []; ct = Text.Def_func_t ft } as
                sub_type ) ) ->
          let id = Dynarray.length declared_types in
          Typetbl.add all_func_types ft id;
          add_sub_type ~name ~sub_type ~declared_types ~named_types ~field_names
        | Text.Typedef.SimpleType (name, sub_type) ->
          add_sub_type ~name ~sub_type ~declared_types ~named_types ~field_names
        | Text.Typedef.RecType members ->
          list_iter
            (fun (name, sub_type) ->
              add_sub_type ~name ~sub_type ~declared_types ~named_types
                ~field_names )
            members )
      typ
  in
  Array.iter
    (fun func_type ->
      match Typetbl.find_opt all_func_types func_type with
      | Some _id -> ()
      | None ->
        let id = Dynarray.length declared_types in
        let sub_type =
          { Text.final = true; ids = []; ct = Text.Def_func_t func_type }
        in
        Dynarray.add_last declared_types sub_type;
        Typetbl.add all_func_types func_type id )
    decl_types;
  (* decl_types contains implicitly declared function types *)
  Ok (Dynarray.to_array declared_types, named_types, field_names)

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

let check_type_id (types : Text.sub_type Array.t)
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
    match Array.unsafe_get types id with
    | Text.{ ct = Def_func_t func_type'; _ } ->
      if not (Text.func_type_eq func_type func_type') then
        Error `Inline_function_type
      else Ok ()
    | _ -> Error `Inline_function_type

let of_grouped
  ({ global
   ; table
   ; mem
   ; func
   ; elem
   ; data
   ; type_checks
   ; typ
   ; decl_types
   ; tag
   ; _
   } :
    Grouped.t ) : t Result.t =
  Log.debug (fun m -> m "assigning    ...");
  let* typ, typ_names, field_names = assign_types typ decl_types in
  let* global_names =
    name "global"
      ~get_name:(get_origin_name (fun ({ id; _ } : Text.Global.t) -> id))
      global
  in
  let* table_names =
    name "table"
      ~get_name:(get_origin_name (fun ({ id; _ } : Text.Table.t) -> id))
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
  let* tag_names =
    name "tag"
      ~get_name:(get_origin_name (fun ({ id; _ } : Text.Tag.t) -> id))
      tag
  in
  let+ () = array_iter (check_type_id typ typ_names) type_checks in

  let modul =
    { typ
    ; typ_names
    ; field_names
    ; global_names
    ; table_names
    ; mem_names
    ; func_names
    ; elem_names
    ; data_names
    ; tag_names
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

let find_tag modul id = find modul.tag_names (`Unknown_tag id) id

let get_func_type modul idx =
  if idx >= Array.length modul.typ then None
  else
    match Array.unsafe_get modul.typ idx with
    | Text.{ ct = Def_func_t ft; _ } -> Some ft
    | _ -> None

let get_types modul = modul.typ

let find_raw_func_type modul func_type =
  let ty_opt =
    Array.find_index
      (function
        | Text.{ ct = Def_func_t ft; _ } -> Text.func_type_eq func_type ft
        | _ -> false )
      modul.typ
  in
  match ty_opt with None -> assert false | Some idx -> idx

let find_field modul type_idx field_id =
  match field_id with
  | Text.Raw i -> Ok i
  | Text name -> (
    match Hashtbl.find_opt modul.field_names type_idx with
    | None ->
      Error
        (`Type_mismatch (Fmt.str "no named fields for struct type %d" type_idx))
    | Some tbl -> (
      match Hashtbl.find_opt tbl name with
      | None ->
        Error
          (`Type_mismatch
             (Fmt.str "unknown field %s in struct type %d" name type_idx) )
      | Some i -> Ok i ) )
