open Syntax
open Simplified

let equal_func_types (a : func_type) (b : func_type) : bool =
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

type tbl = (string, int) Hashtbl.t Option.t

let convert_heap_type tbl = function
  | Symbolic.Any_ht -> Ok Any_ht
  | None_ht -> Ok None_ht
  | Eq_ht -> Ok Eq_ht
  | I31_ht -> Ok I31_ht
  | Struct_ht -> Ok Struct_ht
  | Array_ht -> Ok Array_ht
  | Func_ht -> Ok Func_ht
  | No_func_ht -> Ok No_func_ht
  | Extern_ht -> Ok Extern_ht
  | No_extern_ht -> Ok No_extern_ht
  | Def_ht (Raw i) -> ok @@ Def_ht i
  | Def_ht (Symbolic i) -> begin
    match tbl with
    | None -> error_s "unknown type %s (no table)" i
    | Some tbl -> begin
      match Hashtbl.find_opt tbl i with
      | None -> error_s "unknown type %s (not found in table)" i
      | Some i -> ok @@ Def_ht i
    end
  end

let convert_ref_type tbl (null, heap_type) =
  let+ heap_type = convert_heap_type tbl heap_type in
  (null, heap_type)

let convert_val_type tbl = function
  | Symbolic.Num_type t -> ok @@ Num_type t
  | Ref_type rt ->
    let+ rt = convert_ref_type tbl rt in
    Ref_type rt

let convert_param tbl (n, t) =
  let+ t = convert_val_type tbl t in
  (n, t)

let convert_pt tbl l = list_map (convert_param tbl) l

let convert_rt tbl l = list_map (convert_val_type tbl) l

let convert_func_type tbl (pt, rt) =
  let* pt = convert_pt tbl pt in
  let+ rt = convert_rt tbl rt in
  (pt, rt)

let convert_storage_type tbl = function
  | Symbolic.Val_storage_t val_type ->
    let+ val_type = convert_val_type tbl val_type in
    Val_storage_t val_type
  | Val_packed_t packed_type -> ok @@ Val_packed_t packed_type

let convert_field_type tbl (mut, storage_type) =
  let+ storage_type = convert_storage_type tbl storage_type in
  (mut, storage_type)

let convert_struct_field tbl (id, types) =
  let+ types = list_map (convert_field_type tbl) types in
  (id, types)

let convert_struct_type tbl fields = list_map (convert_struct_field tbl) fields

let convert_str tbl = function
  | Symbolic.Def_func_t func_t ->
    let+ func_t = convert_func_type tbl func_t in
    Def_func_t func_t
  | Def_array_t field_t ->
    let+ field_t = convert_field_type tbl field_t in
    Def_array_t field_t
  | Def_struct_t struct_t ->
    let+ struct_t = convert_struct_type tbl struct_t in
    Def_struct_t struct_t

let convert_table_type tbl (limits, ref_type) =
  let+ ref_type = convert_ref_type tbl ref_type in
  (limits, ref_type)
