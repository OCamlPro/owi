(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

let sp ppf () = Fmt.char ppf ' '

(* identifiers *)

type indice = int

let pp_indice ppf i = int ppf i

let pp_indice_not0 ppf i = if i <> 0 then Fmt.pf ppf " %d" i

let pp_str_opt ppf = function None -> () | Some i -> pf ppf " %s" i

(** Structure *)

(** Types *)

type heap_type =
  | TypeUse of indice
  (* abs_heap_type *)
  | Any_ht
  | Eq_ht
  | I31_ht
  | Struct_ht
  | Array_ht
  | None_ht
  | Func_ht
  | NoFunc_ht
  | Exn_ht
  | NoExn_ht
  | Extern_ht
  | NoExtern_ht

let pp_heap_type fmt = function
  | TypeUse id -> pf fmt "%a" pp_indice id
  | Any_ht -> pf fmt "any"
  | Eq_ht -> pf fmt "eq"
  | I31_ht -> pf fmt "i31"
  | Struct_ht -> pf fmt "struct"
  | Array_ht -> pf fmt "any"
  | None_ht -> pf fmt "none"
  | Func_ht -> pf fmt "func"
  | NoFunc_ht -> pf fmt "nofunc"
  | Exn_ht -> pf fmt "exn"
  | NoExn_ht -> pf fmt "noexn"
  | Extern_ht -> pf fmt "extern"
  | NoExtern_ht -> pf fmt "noextern"

let heap_type_eq t1 t2 =
  (* TODO: this is wrong *)
  match (t1, t2) with
  | Func_ht, Func_ht | Extern_ht, Extern_ht -> true
  | TypeUse id1, TypeUse id2 -> Int.equal id1 id2
  | _, _ -> false

type ref_type = Text.nullable * heap_type

let pp_ref_type ppf (n, ht) =
  match n with
  | Text.No_null -> pf ppf "(ref %a)" pp_heap_type ht
  | Null -> pf ppf "(ref null %a)" pp_heap_type ht

let ref_type_eq t1 t2 =
  match (t1, t2) with
  | ((Text.Null : Text.nullable), t1), ((Text.Null : Text.nullable), t2)
  | (No_null, t1), (No_null, t2) ->
    heap_type_eq t1 t2
  | _ -> false

type val_type =
  | Num_type of Text.num_type
  | Ref_type of ref_type

let pp_val_type fmt = function
  | Num_type t -> Text.pp_num_type fmt t
  | Ref_type t -> pp_ref_type fmt t

let val_type_eq t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> Text.num_type_eq t1 t2
  | Ref_type t1, Ref_type t2 -> ref_type_eq t1 t2
  | _, _ -> false

type storage_type =
  | Val_type of val_type
  | Pack_type of Text.pack_type

let pp_storage_type fmt = function
  | Val_type vt -> pp_val_type fmt vt
  | Pack_type I8 -> Fmt.pf fmt "i8"
  | Pack_type I16 -> Fmt.pf fmt "i16"

let is_subtype_ref_type t1 t2 =
  match (t1, t2) with
  | (Text.No_null, ht1), ((Text.Null : Text.nullable), ht2)
    when heap_type_eq ht1 ht2 ->
    true
  | (No_null, TypeUse _), (Null, Func_ht)
  | (No_null, TypeUse _), (No_null, Func_ht)
  | (Null, TypeUse _), (Null, Func_ht) ->
    true
  | (Null, t1), (Null, t2) | (No_null, t1), (No_null, t2) -> heap_type_eq t1 t2
  | _ -> false

let is_subtype_val_type t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> Text.num_type_eq t1 t2
  | Ref_type t1, Ref_type t2 -> is_subtype_ref_type t1 t2
  | _, _ -> false

type param = string option * val_type

let pp_param ppf ((id, vt) : param) =
  pf ppf "(param%a %a)" pp_str_opt id pp_val_type vt

let param_eq (_, t1) (_, t2) = val_type_eq t1 t2

type param_type = param list

let pp_param_type ppf (params : param_type) = list ~sep:sp pp_param ppf params

let param_type_eq t1 t2 = List.equal param_eq t1 t2

type result_type = val_type list

let result_type_eq t1 t2 = List.equal val_type_eq t1 t2

let pp_result_ ppf vt = pf ppf "(result %a)" pp_val_type vt

let pp_result_type ppf results = list ~sep:sp pp_result_ ppf results

let with_space_list printer ppf l =
  match l with [] -> () | _l -> pf ppf " %a" printer l

type func_type = param_type * result_type

let pp_func_type ppf (params, results) =
  pf ppf "(func%a%a)"
    (with_space_list pp_param_type)
    params
    (with_space_list pp_result_type)
    results

let func_type_eq (pt1, rt1) (pt2, rt2) =
  param_type_eq pt1 pt2 && result_type_eq rt1 rt2

type field_type = Text.mut * storage_type

let pp_field_type fmt (m, st) =
  Fmt.pf fmt "%a %a" Text.pp_mut m pp_storage_type st

type field = indice option * field_type

let pp_field fmt (id_opt, ft) =
  match id_opt with
  | None -> Fmt.pf fmt "%a" pp_field_type ft
  | Some id -> Fmt.pf fmt "%a %a" pp_indice id pp_field_type ft

type comp_type =
  | Def_struct_t of field list
  | Def_array_t of field_type
  | Def_func_t of func_type

let comp_type_eq ct1 ct2 =
  match (ct1, ct2) with
  | Def_struct_t _, Def_struct_t _ -> assert false
  | Def_array_t _, Def_array_t _ -> assert false
  | Def_func_t _, Def_func_t _ -> assert false
  | _ -> false

let pp_comp_type fmt = function
  | Def_struct_t fl ->
    Fmt.pf fmt "(struct %a)" (Fmt.list ~sep:Fmt.comma pp_field) fl
  | Def_array_t ft -> Fmt.pf fmt "(array %a)" pp_field_type ft
  | Def_func_t ft -> Fmt.pf fmt "%a" pp_func_type ft
(* TODO: ensure proper printing *)

type sub_type =
  { final : bool
  ; ids : indice list
  ; ct : comp_type
  }

let sub_type_eq { final = f1; ct = ct1; _ } { final = f2; ct = ct2; _ } =
  Bool.equal f1 f2 && comp_type_eq ct1 ct2

let pp_sub_type fmt { final; ids; ct } =
  Fmt.pf fmt "%a%a%a"
    (fun fmt b -> if b && not (List.is_empty ids) then Fmt.pf fmt "final ")
    final
    (Fmt.list (fun fmt id -> Fmt.pf fmt "%a " pp_indice id))
    ids pp_comp_type ct

type block_type =
  (* TODO: inline this *)
  | Bt_raw of (indice option * func_type)

type nonrec memarg =
  { offset : Int64.t
  ; align : Int32.t
  }

let pp_memarg =
  let pow_2 n =
    assert (Int32.le 0l n);
    Int32.shl 1l n
  in
  fun ppf { offset; align } ->
    let pp_offset ppf offset =
      if Int64.lt_u 0L offset then pf ppf " offset=%Ld" offset
    in
    pf ppf "%a align=%ld" pp_offset offset (pow_2 align)

(* wrap printer to print a space before a non empty list *)
(* TODO or make it an optional arg of pp_list? *)
let with_space_list printer ppf l =
  match l with [] -> () | _l -> pf ppf " %a" printer l

let pp_block_type ppf = function
  | Bt_raw (_ind, (pt, rt)) ->
    pf ppf "%a%a"
      (with_space_list pp_param_type)
      pt
      (with_space_list pp_result_type)
      rt

(** Instructions *)
let pp_block_type_opt ppf = function
  | None -> ()
  | Some bt -> pp_block_type ppf bt

(** I32 instructions *)

type i32_instr =
  | Const of Int32.t
  | Clz
  | Ctz
  | Popcnt
  | Add
  | Sub
  | Mul
  | Div of Text.sx
  | Rem of Text.sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of Text.sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of Text.sx
  | Gt of Text.sx
  | Le of Text.sx
  | Ge of Text.sx
  | Extend8_s
  | Extend16_s
  | Wrap_i64
  | Trunc_f of Text.nn * Text.sx
  | Trunc_sat_f of Text.nn * Text.sx
  | Reinterpret_f of Text.nn
  | Load of indice * memarg
  | Load8 of indice * Text.sx * memarg
  | Load16 of indice * Text.sx * memarg
  | Store of indice * memarg
  | Store8 of indice * memarg
  | Store16 of indice * memarg

let pp_i32_instr ppf = function
  | Const i -> pf ppf "i32.const %ld" i
  | Clz -> pf ppf "i32.clz"
  | Ctz -> pf ppf "i32.ctz"
  | Popcnt -> pf ppf "i32.popcnt"
  | Add -> pf ppf "i32.add"
  | Sub -> pf ppf "i32.sub"
  | Mul -> pf ppf "i32.mul"
  | Div sx -> pf ppf "i32.div_%a" Text.pp_sx sx
  | Rem sx -> pf ppf "i32.rem_%a" Text.pp_sx sx
  | And -> pf ppf "i32.and"
  | Or -> pf ppf "i32.or"
  | Xor -> pf ppf "i32.xor"
  | Shl -> pf ppf "i32.shl"
  | Shr sx -> pf ppf "i32.shr_%a" Text.pp_sx sx
  | Rotl -> pf ppf "i32.rotl"
  | Rotr -> pf ppf "i32.rotr"
  | Eqz -> pf ppf "i32.eqz"
  | Eq -> pf ppf "i32.eq"
  | Ne -> pf ppf "i32.ne"
  | Lt sx -> pf ppf "i32.lt_%a" Text.pp_sx sx
  | Gt sx -> pf ppf "i32.gt_%a" Text.pp_sx sx
  | Le sx -> pf ppf "i32.le_%a" Text.pp_sx sx
  | Ge sx -> pf ppf "i32.ge_%a" Text.pp_sx sx
  | Extend8_s -> pf ppf "i32.extend8_s"
  | Extend16_s -> pf ppf "i32.extend16_s"
  | Wrap_i64 -> pf ppf "i32.wrap_i64"
  | Trunc_f (nn, sx) -> pf ppf "i32.trunc_f%a_%a" Text.pp_nn nn Text.pp_sx sx
  | Trunc_sat_f (nn, sx) ->
    pf ppf "i32.truc_sat_f%a_%a" Text.pp_nn nn Text.pp_sx sx
  | Reinterpret_f nn -> pf ppf "i32.reinterpret_f%a" Text.pp_nn nn
  | Load (indice, memarg) ->
    pf ppf "i32.load%a%a" pp_indice_not0 indice pp_memarg memarg
  | Load8 (indice, sx, memarg) ->
    pf ppf "i32.load8_%a%a%a" Text.pp_sx sx pp_indice_not0 indice pp_memarg
      memarg
  | Load16 (indice, sx, memarg) ->
    pf ppf "i32.load16_%a%a%a" Text.pp_sx sx pp_indice_not0 indice pp_memarg
      memarg
  | Store (indice, memarg) ->
    pf ppf "i32.store%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store8 (indice, memarg) ->
    pf ppf "i32.store8%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store16 (indice, memarg) ->
    pf ppf "i32.store16%a%a" pp_indice_not0 indice pp_memarg memarg

(** I64 instructions *)

type i64_instr =
  | Const of Int64.t
  | Clz
  | Ctz
  | Popcnt
  | Add
  | Sub
  | Mul
  | Div of Text.sx
  | Rem of Text.sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of Text.sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of Text.sx
  | Gt of Text.sx
  | Le of Text.sx
  | Ge of Text.sx
  | Extend8_s
  | Extend16_s
  | Extend32_s
  | Extend_i32 of Text.sx
  | Trunc_f of Text.nn * Text.sx
  | Trunc_sat_f of Text.nn * Text.sx
  | Reinterpret_f of Text.nn
  | Load of indice * memarg
  | Load8 of indice * Text.sx * memarg
  | Load16 of indice * Text.sx * memarg
  | Load32 of indice * Text.sx * memarg
  | Store of indice * memarg
  | Store8 of indice * memarg
  | Store16 of indice * memarg
  | Store32 of indice * memarg

let pp_i64_instr ppf = function
  | Const i -> pf ppf "i64.const %Ld" i
  | Clz -> pf ppf "i64.clz"
  | Ctz -> pf ppf "i64.ctz"
  | Popcnt -> pf ppf "i64.popcnt"
  | Add -> pf ppf "i64.add"
  | Sub -> pf ppf "i64.sub"
  | Mul -> pf ppf "i64.mul"
  | Div sx -> pf ppf "i64.div_%a" Text.pp_sx sx
  | Rem sx -> pf ppf "i64.rem_%a" Text.pp_sx sx
  | And -> pf ppf "i64.and"
  | Or -> pf ppf "i64.or"
  | Xor -> pf ppf "i64.xor"
  | Shl -> pf ppf "i64.shl"
  | Shr sx -> pf ppf "i64.shr_%a" Text.pp_sx sx
  | Rotl -> pf ppf "i64.rotl"
  | Rotr -> pf ppf "i64.rotr"
  | Eqz -> pf ppf "i64.eqz"
  | Eq -> pf ppf "i64.eq"
  | Ne -> pf ppf "i64.ne"
  | Lt sx -> pf ppf "i64.lt_%a" Text.pp_sx sx
  | Gt sx -> pf ppf "i64.gt_%a" Text.pp_sx sx
  | Le sx -> pf ppf "i64.le_%a" Text.pp_sx sx
  | Ge sx -> pf ppf "i64.ge_%a" Text.pp_sx sx
  | Extend8_s -> pf ppf "i64.extend8_s"
  | Extend16_s -> pf ppf "i64.extend16_s"
  | Extend32_s -> pf ppf "i64.extend32_s"
  | Extend_i32 sx -> pf ppf "i64.extend_i32_%a" Text.pp_sx sx
  | Trunc_f (nn, sx) -> pf ppf "i64.trunc_f%a_%a" Text.pp_nn nn Text.pp_sx sx
  | Trunc_sat_f (nn, sx) ->
    pf ppf "i64.trunc_sat_f%a_%a" Text.pp_nn nn Text.pp_sx sx
  | Reinterpret_f nn -> pf ppf "i64.reinterpret_f%a" Text.pp_nn nn
  | Load (indice, memarg) ->
    pf ppf "i64.load%a%a" pp_indice_not0 indice pp_memarg memarg
  | Load8 (indice, sx, memarg) ->
    pf ppf "i64.load8_%a%a%a" pp_indice_not0 indice Text.pp_sx sx pp_memarg
      memarg
  | Load16 (indice, sx, memarg) ->
    pf ppf "i64.load16_%a%a%a" pp_indice_not0 indice Text.pp_sx sx pp_memarg
      memarg
  | Load32 (indice, sx, memarg) ->
    pf ppf "i64.load32_%a%a%a" pp_indice_not0 indice Text.pp_sx sx pp_memarg
      memarg
  | Store (indice, memarg) ->
    pf ppf "i64.store%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store8 (indice, memarg) ->
    pf ppf "i64.store8%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store16 (indice, memarg) ->
    pf ppf "i64.store16%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store32 (indice, memarg) ->
    pf ppf "i64.store32%a%a" pp_indice_not0 indice pp_memarg memarg

(** F32 instructions *)

type f32_instr =
  | Const of Float32.t
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Demote_f64
  | Convert_i of Text.nn * Text.sx
  | Reinterpret_i of Text.nn
  | Load of indice * memarg
  | Store of indice * memarg

let pp_f32_instr ppf = function
  | Const f -> pf ppf "f32.const %a" Float32.pp f
  | Abs -> pf ppf "f32.abs"
  | Neg -> pf ppf "f32.neg"
  | Sqrt -> pf ppf "f32.sqrt"
  | Ceil -> pf ppf "f32.ceil"
  | Floor -> pf ppf "f32.floor"
  | Trunc -> pf ppf "f32.trunc"
  | Nearest -> pf ppf "f32.nearest"
  | Add -> pf ppf "f32.add"
  | Sub -> pf ppf "f32.sub"
  | Mul -> pf ppf "f32.mul"
  | Div -> pf ppf "f32.div"
  | Min -> pf ppf "f32.min"
  | Max -> pf ppf "f32.max"
  | Copysign -> pf ppf "f32.copysign"
  | Eq -> pf ppf "f32.eq"
  | Ne -> pf ppf "f32.ne"
  | Lt -> pf ppf "f32.lt"
  | Gt -> pf ppf "f32.gt"
  | Le -> pf ppf "f32.le"
  | Ge -> pf ppf "f32.ge"
  | Demote_f64 -> pf ppf "f32.demote_f64"
  | Convert_i (nn, sx) -> pf ppf "f32.convert_i%a%a" Text.pp_nn nn Text.pp_sx sx
  | Reinterpret_i nn -> pf ppf "f32.reinterpret_i%a" Text.pp_nn nn
  | Load (indice, memarg) ->
    pf ppf "f32.load%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store (indice, memarg) ->
    pf ppf "f32.store%a%a" pp_indice_not0 indice pp_memarg memarg

(** F64 instructions *)

type f64_instr =
  | Const of Float64.t
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Promote_f32
  | Convert_i of Text.nn * Text.sx
  | Reinterpret_i of Text.nn
  | Load of indice * memarg
  | Store of indice * memarg

let pp_f64_instr ppf = function
  | Const f -> pf ppf "f64.const %a" Float64.pp f
  | Abs -> pf ppf "f64.abs"
  | Neg -> pf ppf "f64.neg"
  | Sqrt -> pf ppf "f64.sqrt"
  | Ceil -> pf ppf "f64.ceil"
  | Floor -> pf ppf "f64.floor"
  | Trunc -> pf ppf "f64.trunc"
  | Nearest -> pf ppf "f64.nearest"
  | Add -> pf ppf "f64.add"
  | Sub -> pf ppf "f64.sub"
  | Mul -> pf ppf "f64.mul"
  | Div -> pf ppf "f64.div"
  | Min -> pf ppf "f64.min"
  | Max -> pf ppf "f64.max"
  | Copysign -> pf ppf "f64.copysign"
  | Eq -> pf ppf "f64.eq"
  | Ne -> pf ppf "f64.ne"
  | Lt -> pf ppf "f64.lt"
  | Gt -> pf ppf "f64.gt"
  | Le -> pf ppf "f64.le"
  | Ge -> pf ppf "f64.ge"
  | Promote_f32 -> pf ppf "f64.promote_f32"
  | Convert_i (nn, sx) ->
    pf ppf "f64.convert_i%a_%a" Text.pp_nn nn Text.pp_sx sx
  | Reinterpret_i nn -> pf ppf "f64.reinterpret_i%a" Text.pp_nn nn
  | Load (indice, memarg) ->
    pf ppf "f64.load%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store (indice, memarg) ->
    pf ppf "f64.store%a%a" pp_indice_not0 indice pp_memarg memarg

(** Reference instructions *)
type ref_instr =
  | Null of heap_type
  | Is_null
  | As_non_null
  | Func of indice

let pp_ref_instr ppf = function
  | Null t -> pf ppf "ref.null %a" pp_heap_type t
  | Is_null -> pf ppf "ref.is_null"
  | As_non_null -> pf ppf "ref.as_non_null"
  | Func indice -> pf ppf "ref.func %a" pp_indice indice

(* Local instructions *)
type local_instr =
  | Get of indice
  | Set of indice
  | Tee of indice

let pp_local_instr ppf = function
  | Get indice -> pf ppf "local.get %a" pp_indice indice
  | Set indice -> pf ppf "local.set %a" pp_indice indice
  | Tee indice -> pf ppf "local.tee %a" pp_indice indice

(** Global instructions *)
type global_instr =
  | Get of indice
  | Set of indice

let pp_global_instr ppf = function
  | Get indice -> pf ppf "global.get %a" pp_indice indice
  | Set indice -> pf ppf "global.set %a" pp_indice indice

(** Table instructions *)
type table_instr =
  | Get of indice
  | Set of indice
  | Size of indice
  | Grow of indice
  | Fill of indice
  | Copy of indice * indice
  | Init of indice * indice

let pp_table_instr ppf = function
  | Get indice -> pf ppf "table.get %a" pp_indice indice
  | Set indice -> pf ppf "table.set %a" pp_indice indice
  | Size indice -> pf ppf "table.size %a" pp_indice indice
  | Grow indice -> pf ppf "table.grow %a" pp_indice indice
  | Fill indice -> pf ppf "table.fill %a" pp_indice indice
  | Copy (indice, indice') ->
    pf ppf "table.copy %a %a" pp_indice indice pp_indice indice'
  | Init (table_indice, elem_indice) ->
    pf ppf "table.init %a %a" pp_indice table_indice pp_indice elem_indice

(** Elem instructions *)
type elem_instr = Drop of indice

let pp_elem_instr ppf = function
  | Drop indice -> pf ppf "elem.drop %a" pp_indice indice

(** Memory instructions *)
type memory_instr =
  | Size of indice
  | Grow of indice
  | Fill of indice
  | Copy of indice * indice
  | Init of indice * indice

let pp_memory_instr ppf = function
  | Size indice -> pf ppf "memory.size%a" pp_indice_not0 indice
  | Grow indice -> pf ppf "memory.grow%a" pp_indice_not0 indice
  | Fill indice -> pf ppf "memory.fill%a" pp_indice_not0 indice
  | Copy (0, 0) -> pf ppf "memory.copy"
  | Copy (indice1, indice2) ->
    pf ppf "memory.copy %a %a" pp_indice indice1 pp_indice indice2
  | Init (mem_indice, data_indice) ->
    pf ppf "memory.init%a %a" pp_indice_not0 mem_indice pp_indice data_indice

(** Data instructions *)
type data_instr = Drop of indice

let pp_data_instr ppf = function
  | Drop indice -> pf ppf "data.drop %a" pp_indice indice

(** Instructions *)
type instr =
  | I32 of i32_instr
  | I64 of i64_instr
  | F32 of f32_instr
  | F64 of f64_instr
  | V128 of Text.v128_instr
  | I8x16 of Text.i8x16_instr
  | I16x8 of Text.i16x8_instr
  | I32x4 of Text.i32x4_instr
  | I64x2 of Text.i64x2_instr
  | Ref of ref_instr
  | Local of local_instr
  | Global of global_instr
  | Table of table_instr
  | Elem of elem_instr
  | Memory of memory_instr
  | Data of data_instr
  (* Reference instructions *)
  | Ref_eq
  | Ref_test of ref_type
  | Ref_cast of ref_type
  (* Parametric instructions *)
  | Drop
  | Select of val_type list option
  | Nop
  | Unreachable
  | Block of string option * block_type option * expr Annotated.t
  | Loop of string option * block_type option * expr Annotated.t
  | If_else of
      string option * block_type option * expr Annotated.t * expr Annotated.t
  | Br of indice
  | Br_if of indice
  | Br_table of indice array * indice
  | Br_on_null of indice
  | Br_on_non_null of indice
  | Return
  | Return_call of indice
  | Return_call_indirect of indice * block_type
  | Return_call_ref of block_type
  | Call of indice
  | Call_indirect of indice * block_type
  | Call_ref of indice
  (* aggregate types *)
  (* i31 *)
  | Ref_i31
  | I31_get_s
  | I31_get_u
  (* struct *)
  | Struct_new of indice
  | Struct_new_default of indice
  | Struct_get of indice * indice
  | Struct_get_s of indice * indice
  | Struct_get_u of indice * indice
  | Struct_set of indice * indice
  (* array *)
  | Array_new of indice
  | Array_new_default of indice
  | Array_new_fixed of indice * Int32.t
  | Array_new_data of indice * indice
  | Array_new_elem of indice * indice
  | Array_get of indice
  | Array_get_s of indice
  | Array_get_u of indice
  | Array_set of indice
  | Array_len
  | Array_fill of indice
  | Array_copy of indice * indice
  | Array_init_data of indice * indice
  | Array_init_elem of indice * indice
  (* convesion *)
  | Any_convert_extern
  | Extern_convert_any

and expr = instr Annotated.t list

let pp_newline ppf () = pf ppf "@\n"

let rec pp_instr ~short ppf = function
  | I32 i -> pp_i32_instr ppf i
  | I64 i -> pp_i64_instr ppf i
  | F32 i -> pp_f32_instr ppf i
  | F64 i -> pp_f64_instr ppf i
  | V128 i -> Text.pp_v128_instr ppf i
  | I8x16 i -> Text.pp_i8x16_instr ppf i
  | I16x8 i -> Text.pp_i16x8_instr ppf i
  | I32x4 i -> Text.pp_i32x4_instr ppf i
  | I64x2 i -> Text.pp_i64x2_instr ppf i
  | Ref i -> pp_ref_instr ppf i
  | Local i -> pp_local_instr ppf i
  | Global i -> pp_global_instr ppf i
  | Table i -> pp_table_instr ppf i
  | Elem i -> pp_elem_instr ppf i
  | Memory i -> pp_memory_instr ppf i
  | Data i -> pp_data_instr ppf i
  | Drop -> pf ppf "drop"
  | Ref_eq -> pf fmt "ref.eq"
  | Ref_test rt -> pf fmt "ref.test %a" pp_ref_type rt
  | Ref_cast rt -> pf fmt "ref.cast %a" pp_ref_type rt
  | Drop -> pf fmt "drop"
  | Select vt ->
    begin match vt with
    | None -> pf ppf "select"
    | Some vt -> pf ppf "select (%a)" pp_result_type vt
    (* TODO: are the parens needed ? *)
    end
  | Nop -> pf ppf "nop"
  | Unreachable -> pf ppf "unreachable"
  | Block (id, bt, e) ->
    if short then pf ppf "block%a%a" Text.pp_id_opt id pp_block_type_opt bt
    else
      pf ppf "(block%a%a@\n  @[<v>%a@])" Text.pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | Loop (id, bt, e) ->
    if short then pf ppf "loop%a%a" Text.pp_id_opt id pp_block_type_opt bt
    else
      pf ppf "(loop%a%a@\n  @[<v>%a@])" Text.pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | If_else (id, bt, e1, e2) ->
    let pp_else ppf e =
      Annotated.iter
        (function
          | [] -> ()
          | _ -> pf ppf "@\n(else@\n  @[<v>%a@]@\n)" (pp_expr ~short) e )
        e
    in
    if short then pf ppf "if%a%a" Text.pp_id_opt id pp_block_type_opt bt
    else
      pf ppf "(if%a%a@\n  @[<v>(then@\n  @[<v>%a@]@\n)%a@]@\n)" Text.pp_id_opt
        id pp_block_type_opt bt (pp_expr ~short) e1 pp_else e2
  | Br id -> pf ppf "br %a" pp_indice id
  | Br_if id -> pf ppf "br_if %a" pp_indice id
  | Br_table (ids, id) ->
    pf ppf "br_table %a %a" (array ~sep:sp pp_indice) ids pp_indice id
  | Br_on_null id -> pf ppf "br_on_null %a" pp_indice id
  | Br_on_non_null id -> pf ppf "br_on_non_null %a" pp_indice id
  | Return -> pf ppf "return"
  | Return_call id -> pf ppf "return_call %a" pp_indice id
  | Return_call_indirect (tbl_id, ty_id) ->
    pf ppf "return_call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
  | Return_call_ref ty_id -> pf ppf "return_call_ref %a" pp_block_type ty_id
  | Call id -> pf ppf "call %a" pp_indice id
  | Call_indirect (tbl_id, ty_id) ->
    pf fmt "call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
  | Call_ref ty_id -> pf fmt "call_ref %a" pp_indice ty_id
  | Ref_i31 -> pf fmt "ref.i31"
  | I31_get_s -> pf fmt "i31.get_s"
  | I31_get_u -> pf fmt "i31.get_u"
  | Struct_new id -> pf fmt "struct.new %a" pp_indice id
  | Struct_new_default id -> pf fmt "struct.new_default %a" pp_indice id
  | Struct_get (id1, id2) ->
    pf fmt "struct.get %a %a" pp_indice id1 pp_indice id2
  | Struct_get_s (id1, id2) ->
    pf fmt "struct.get_s %a %a" pp_indice id1 pp_indice id2
  | Struct_get_u (id1, id2) ->
    pf fmt "struct.get_u %a %a" pp_indice id1 pp_indice id2
  | Struct_set (id1, id2) ->
    pf fmt "struct.set %a %a" pp_indice id1 pp_indice id2
  | Array_new id -> pf fmt "array.new %a" pp_indice id
  | Array_new_default id -> pf fmt "array.new_default %a" pp_indice id
  | Array_new_fixed (id, n) -> pf fmt "array.new_fixed %a %ld" pp_indice id n
  | Array_new_data (id1, id2) ->
    pf fmt "array.new_data %a %a" pp_indice id1 pp_indice id2
  | Array_new_elem (id1, id2) ->
    pf fmt "array.new_elem %a %a" pp_indice id1 pp_indice id2
  | Array_get id -> pf fmt "array.get %a" pp_indice id
  | Array_get_s id -> pf fmt "array.get_s %a" pp_indice id
  | Array_get_u id -> pf fmt "array.get_u %a" pp_indice id
  | Array_set id -> pf fmt "array.set %a" pp_indice id
  | Array_len -> pf fmt "array.len"
  | Array_fill id -> pf fmt "array.fill %a" pp_indice id
  | Array_copy (id1, id2) ->
    pf fmt "array.copy %a %a" pp_indice id1 pp_indice id2
  | Array_init_data (id1, id2) ->
    pf fmt "array.init_data %a %a" pp_indice id1 pp_indice id2
  | Array_init_elem (id1, id2) ->
    pf fmt "array.init_elem %a %a" pp_indice id1 pp_indice id2
  | Any_convert_extern -> pf fmt "any.convert_extern"
  | Extern_convert_any -> pf fmt "extern.convert_any"

and pp_expr ~short ppf instrs =
  Annotated.iter
    (fun instrs ->
      list ~sep:pp_newline
        (fun ppf i -> Annotated.iter (pp_instr ~short ppf) i)
        ppf instrs )
    instrs

let rec iter_expr f (e : expr Annotated.t) =
  Annotated.iter (List.iter (iter_instr f)) e

and iter_instr f instr =
  Annotated.iter f instr;
  Annotated.iter
    (function
      | I32 _ | I64 _ | F32 _ | F64 _ | V128 _ | I8x16 _ | I16x8 _ | I32x4 _
      | I64x2 _ | Ref _ | Local _ | Global _ | Table _ | Elem _ | Memory _
      | Data _ | Drop | Select _ | Nop | Unreachable | Br _ | Br_if _
      | Br_table (_, _)
      | Br_on_null _ | Br_on_non_null _ | Return | Return_call _
      | Return_call_indirect (_, _)
      | Return_call_ref _ | Call _
      | Call_indirect (_, _)
      | Call_ref _ | Ref_i31 | I31_get_s | I31_get_u | Struct_new _
      | Struct_new_default _
      | Struct_get (_, _)
      | Struct_get_s (_, _)
      | Struct_get_u (_, _)
      | Struct_set (_, _)
      | Array_new _ | Array_new_default _
      | Array_new_fixed (_, _)
      | Array_new_data (_, _)
      | Array_new_elem (_, _)
      | Array_get _ | Array_get_s _ | Array_get_u _ | Array_set _ | Array_len
      | Array_fill _
      | Array_copy (_, _)
      | Array_init_data (_, _)
      | Array_init_elem (_, _)
      | Any_convert_extern | Extern_convert_any | Ref_eq | Ref_test _
      | Ref_cast _ ->
        ()
      | Block (_, _, e) | Loop (_, _, e) -> iter_expr f e
      | If_else (_, _, e1, e2) ->
        iter_expr f e1;
        iter_expr f e2 )
    instr

module Func = struct
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }
end

(* Modules *)

(* Tags *)
module Tag = struct
  type t =
    { id : string option
    ; typ : block_type
    }
end

(** named export *)
module Export = struct
  type t =
    { name : string
    ; id : int
    }
end

module Typedef = struct
  type t =
    | SimpleType of (string option * sub_type)
    | RecType of (string option * sub_type) list

  let pp_ty fmt (id, ty) =
    pf fmt "(type%a %a)" (Fmt.option Fmt.string) id pp_sub_type ty

  let pp fmt t =
    match t with
    | SimpleType (id, ty) -> Fmt.pf fmt "%a" pp_ty (id, ty)
    | RecType tyl -> Fmt.pf fmt "(rec %a)" (Fmt.list ~sep:Fmt.sp pp_ty) tyl
end

module Table = struct
  module Type = struct
    type limits =
      | I32 of
          { min : Int32.t
          ; max : Int32.t option
          }
      | I64 of
          { min : Int64.t
          ; max : Int64.t option
          }

    let pp_limits ppf = function
      | I32 { min; max = None } -> pf ppf "%ld" min
      | I32 { min; max = Some max } -> pf ppf "%ld %ld" min max
      | I64 { min; max = None } -> pf ppf "i64 %Ld" min
      | I64 { min; max = Some max } -> pf ppf "i64 %Ld %Ld" min max

    type nonrec t = limits * ref_type

    let pp ppf (limits, ref_type) =
      pf ppf "%a %a" pp_limits limits pp_ref_type ref_type
  end

  type t =
    { id : string option
    ; typ : Type.t
    ; init : expr Annotated.t option
    }
end

module Mem = struct
  module Type = struct
    type limits =
      | I32 of
          { min : Int32.t
          ; max : Int32.t option
          }
      | I64 of
          { min : int
          ; max : int option
          }

    let pp_limits ppf = function
      | I32 { min; max = None } -> pf ppf "%ld" min
      | I32 { min; max = Some max } -> pf ppf "%ld %ld" min max
      | I64 { min; max = None } -> pf ppf "i64 %d" min
      | I64 { min; max = Some max } -> pf ppf "i64 %d %d" min max
  end

  type nonrec t = string option * Type.limits

  let pp ppf (id, ty) =
    pf ppf "(memory%a %a)"
      (Fmt.option ~none:Fmt.nop (fun ppf s -> Fmt.pf ppf " %s" s))
      id Type.pp_limits ty
end

module Global = struct
  module Type = struct
    type nonrec t = Text.mut * val_type
  end

  type t =
    { typ : Type.t (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t
    ; id : string option
    }
end

module Data = struct
  module Mode = struct
    type t =
      | Passive
      (* TODO: Data_active binary+const expr*)
      | Active of int * expr Annotated.t
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }
end

module Elem = struct
  module Mode = struct
    type t =
      | Passive
      | Declarative
      (* TODO: Elem_active binary+const expr*)
      | Active of int option * expr Annotated.t
  end

  type t =
    { id : string option
    ; typ : ref_type (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t list
    ; mode : Mode.t
    ; explicit_typ : bool
    }
end

module Custom = struct
  type t = Uninterpreted of string
end

module Module = struct
  module Exports = struct
    type t =
      { global : Export.t Array.t
      ; mem : Export.t Array.t
      ; table : Export.t Array.t
      ; func : Export.t Array.t
      ; tag : Export.t Array.t
      }
  end

  type t =
    { id : string option
    ; types : Typedef.t array
    ; global : (Global.t, Global.Type.t) Origin.t array
    ; table : (Table.t, Table.Type.t) Origin.t array
    ; mem : (Mem.t, Mem.Type.limits) Origin.t array
    ; func : (Func.t, block_type) Origin.t array (* TODO: switch to func_type *)
    ; tag : (Tag.t, block_type) Origin.t array
    ; elem : Elem.t array
    ; data : Data.t array
    ; exports : Exports.t
    ; start : int option
    ; custom : Custom.t list
    }

  let empty =
    { id = None
    ; types = [||]
    ; global = [||]
    ; table = [||]
    ; mem = [||]
    ; func = [||]
    ; elem = [||]
    ; data = [||]
    ; tag = [||]
    ; exports =
        { global = [||]; mem = [||]; table = [||]; func = [||]; tag = [||] }
    ; start = None
    ; custom = []
    }

  (** Functions *)

  (** Insert a function [f] to a module [m] at index [i] and returns the module.
      It will update all function indices accordingly. *)
  let insert_func_at_idx ?(update_function_itself = true) f m i =
    (* TODO: we should also update elements and everything... *)
    (*
    Log.warn (fun m ->
      m "insert_func_at_idx is still incomplete and you may run into issues" );
    *)
    let update_idx idx = if idx >= i then idx + 1 else idx in

    let rec handle_instr instr =
      Annotated.map
        (function
          | Call idx -> Call (update_idx idx)
          | Return_call idx -> Return_call (update_idx idx)
          | Ref (Func idx) -> Ref (Func (update_idx idx))
          | Block (id, typ, body) ->
            let body = handle_expr body in
            Block (id, typ, body)
          | Loop (id, typ, body) ->
            let body = handle_expr body in
            Loop (id, typ, body)
          | If_else (id, typ, true_branch, false_branch) ->
            let true_branch = handle_expr true_branch in
            let false_branch = handle_expr false_branch in
            If_else (id, typ, true_branch, false_branch)
          | instr ->
            (* TODO: make this match non fragile *)
            instr )
        instr
    and handle_expr expr =
      Annotated.map (fun expr -> List.map handle_instr expr) expr
    in
    let update_function = function
      | Origin.Imported _ as f -> f
      | Origin.Local (f : Func.t) ->
        let body = handle_expr f.body in
        Origin.Local { f with body }
    in
    let func =
      Array.init
        (Array.length m.func + 1)
        (fun j ->
          if i = j then if update_function_itself then update_function f else f
          else begin
            update_function @@ if i < j then m.func.(j - 1) else m.func.(j)
          end )
    in
    let elem =
      Array.map
        (fun (elem : Elem.t) ->
          let init = List.map handle_expr elem.init in
          { elem with init } )
        m.elem
    in
    let global =
      Array.map
        (function
          | Origin.Imported _ as v -> v
          | Local (global : Global.t) ->
            let init = handle_expr global.init in
            Local { global with init } )
        m.global
    in

    let start = Option.map update_idx m.start in

    let exports =
      let func =
        Array.map
          (fun export ->
            let id = update_idx (export : Export.t).id in
            { export with id } )
          m.exports.func
      in
      { m.exports with func }
    in

    { m with func; elem; start; global; exports }

  (** Add a function [f] at the end of a module [m] and returns the module and
      the index of the added function. *)
  let add_func f m =
    let len = Array.length m.func in
    let func =
      Array.init
        (Array.length m.func + 1)
        (fun i -> if i = len then f else m.func.(i))
    in

    ({ m with func }, len)

  (** Return the type of the function at index [id]. *)
  let get_func_type id m =
    if id >= Array.length m.func then None
    else
      match m.func.(id) with
      | Local f -> Some f.type_f
      | Imported i -> Some i.typ

  let get_type id m =
    if id >= Array.length m.types then None else Some m.types.(id)

  (** Exports *)

  (** Return the first function exported as [name] if it exists. Return [None]
      otherwise.*)
  let find_exported_func_from_name name m =
    Array.find_opt
      (function { Export.name = name'; _ } -> String.equal name name')
      m.exports.func

  (** Imports *)

  (** Return the index of a function imported from a given [modul_name] and
      [func_name] if it exists. Return [None] otherwise. *)
  let find_imported_func_index ~modul_name ~func_name m =
    Array.find_index
      (function
        | Origin.Imported
            { Origin.modul_name = modul_name'
            ; name
            ; assigned_name = _
            ; typ = _
            } ->
          String.equal modul_name modul_name' && String.equal func_name name
        | Local _ -> false )
      m.func

  (** Finds the index of the last imported function. Will be `~-1` if there are
      no imported functions. *)
  let find_last_import_index m =
    let _i, last =
      Array.fold_left
        (fun (i, last) -> function
          | Origin.Imported _ -> (succ i, i) | Origin.Local _ -> (succ i, last) )
        (0, ~-1) m.func
    in
    last

  (** Look for an imported function index, adding it if not already imported. *)
  let add_import_if_not_present ~modul_name ~func_name ~typ m =
    match find_imported_func_index ~modul_name ~func_name m with
    | Some _i -> m
    | None ->
      let f =
        Origin.imported ~modul_name ~name:func_name ~assigned_name:None ~typ
      in

      let idx = find_last_import_index m + 1 in

      insert_func_at_idx f m idx
end
