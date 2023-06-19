open Simplified

let equal_func_types (a : func_type) (b : func_type) : bool =
  let remove_param (pt, rt) =
    let pt = List.map (fun (_id, vt) -> (None, vt)) pt in
    (pt, rt)
  in
  remove_param a = remove_param b

type tbl = (string, int) Hashtbl.t Option.t

let convert_heap_type tbl : Symbolic.heap_type -> heap_type = function
  | Symbolic.Any_ht -> Any_ht
  | None_ht -> None_ht
  | Eq_ht -> Eq_ht
  | I31_ht -> I31_ht
  | Struct_ht -> Struct_ht
  | Array_ht -> Array_ht
  | Func_ht -> Func_ht
  | No_func_ht -> No_func_ht
  | Extern_ht -> Extern_ht
  | No_extern_ht -> No_extern_ht
  | Def_ht (Raw i) -> Def_ht i
  | Def_ht (Symbolic i) -> begin
    match tbl with
    | None -> assert false
    | Some tbl -> begin
      match Hashtbl.find_opt tbl i with
      | None -> assert false
      | Some i -> Def_ht i
    end
  end

let convert_ref_type tbl : Symbolic.ref_type -> ref_type =
 fun (null, heap_type) -> (null, convert_heap_type tbl heap_type)

let convert_val_type tbl : Symbolic.val_type -> val_type = function
  | Symbolic.Num_type t -> Num_type t
  | Ref_type rt -> Ref_type (convert_ref_type tbl rt)

let convert_param tbl : Symbolic.param -> param =
 fun (n, t) -> (n, convert_val_type tbl t)

let convert_pt tbl : Symbolic.param_type -> param_type =
 fun l -> List.map (convert_param tbl) l

let convert_rt tbl : Symbolic.result_type -> result_type =
 fun l -> List.map (convert_val_type tbl) l

let convert_func_type tbl : Symbolic.func_type -> func_type =
 fun (pt, rt) -> (convert_pt tbl pt, convert_rt tbl rt)

let convert_storage_type tbl : Symbolic.storage_type -> storage_type = function
  | Val_storage_t val_type -> Val_storage_t (convert_val_type tbl val_type)
  | Val_packed_t packed_type -> Val_packed_t packed_type

let convert_field_type tbl : Symbolic.field_type -> field_type =
 fun (mut, storage_type) -> (mut, convert_storage_type tbl storage_type)

let convert_struct_field tbl : Symbolic.struct_field -> struct_field =
 fun (id, types) -> (id, List.map (convert_field_type tbl) types)

let convert_struct_type tbl : Symbolic.struct_type -> struct_type =
 fun fields -> List.map (convert_struct_field tbl) fields

let convert_str tbl : Symbolic.str_type -> str_type = function
  | Def_func_t func_t -> Def_func_t (convert_func_type tbl func_t)
  | Def_array_t field_t -> Def_array_t (convert_field_type tbl field_t)
  | Def_struct_t struct_t -> Def_struct_t (convert_struct_type tbl struct_t)

let convert_table_type tbl : Symbolic.table_type -> table_type =
 fun (limits, ref_type) -> (limits, (convert_ref_type tbl) ref_type)
