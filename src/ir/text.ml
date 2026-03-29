(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

exception Parse_fail of string

let sp ppf () = Fmt.char ppf ' '

(* identifiers *)

type indice =
  | Text of string
  | Raw of int

let pp_id ppf id = pf ppf "$%s" id

let pp_id_opt ppf = function None -> () | Some i -> pf ppf " %a" pp_id i

let pp_indice ppf = function Raw u -> int ppf u | Text i -> pp_id ppf i

let pp_indice_not0 ppf = function
  | Text i -> pp_id ppf i
  | Raw 0 -> ()
  | Raw u -> Fmt.pf ppf " %d" u

let pp_indice_opt ppf = function None -> () | Some i -> pp_indice ppf i

let compare_indice id1 id2 =
  match (id1, id2) with
  | Text s1, Text s2 -> String.compare s1 s2
  | Raw i1, Raw i2 -> Int.compare i1 i2
  | Text _, Raw _ -> 1
  | Raw _, Text _ -> -1

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64
  | V128

let pp_num_type ppf = function
  | I32 -> pf ppf "i32"
  | I64 -> pf ppf "i64"
  | F32 -> pf ppf "f32"
  | F64 -> pf ppf "f64"
  | V128 -> pf ppf "v128"

let num_type_eq t1 t2 =
  match (t1, t2) with
  | I32, I32 | I64, I64 | F32, F32 | F64, F64 | V128, V128 -> true
  | (I32 | I64 | F32 | F64 | V128), _ -> false

let compare_num_type t1 t2 =
  let to_int = function
    | I32 -> 0
    | I64 -> 1
    | F32 -> 2
    | F64 -> 3
    | V128 -> 4
  in
  compare (to_int t1) (to_int t2)

type nullable =
  | No_null
  | Null

type nonrec mut =
  | Const
  | Var

let is_mut = function Const -> false | Var -> true

type nonrec nn =
  | S32
  | S64

let pp_nn ppf = function S32 -> pf ppf "32" | S64 -> pf ppf "64"

type nonrec fshape =
  | F32x4
  | F64x8

type nonrec sx =
  | U
  | S

let pp_sx ppf = function U -> pf ppf "u" | S -> pf ppf "s"

type nonrec memarg =
  { offset : string option
  ; align : string option
  }

let pp_memarg ppf { offset; align } =
  match (offset, align) with
  | None, None -> ()
  | Some offset, Some align -> Fmt.pf ppf " offset=%s align=%s" offset align
  | Some offset, None -> Fmt.pf ppf " offset=%s" offset
  | None, Some align -> Fmt.pf ppf " align=%s" align

type nonrec limits =
  { is_i64 : bool
  ; min : string
  ; max : string option
  }

let pp_addr_type ppf is_i64 = if is_i64 then Fmt.pf ppf "i64 "

let pp_limits ppf { is_i64; min; max } =
  match max with
  | None -> pf ppf "%a%s" pp_addr_type is_i64 min
  | Some max -> pf ppf "%a%s %s" pp_addr_type is_i64 min max

(** Structure *)

(** Types *)

(* TODO: add i31, array, struct and  Eq  *)
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
  | Func_ht, Func_ht
  | Extern_ht, Extern_ht
  | Any_ht, Any_ht
  | None_ht, None_ht
  | NoFunc_ht, NoFunc_ht
  | Exn_ht, Exn_ht
  | NoExn_ht, NoExn_ht
  | NoExtern_ht, NoExtern_ht ->
    true
  | TypeUse id1, TypeUse id2 -> compare_indice id1 id2 = 0
  | _, _ -> false

let compare_heap_type t1 t2 =
  (* TODO: this is wrong *)
  match (t1, t2) with
  | Any_ht, Any_ht
  | Eq_ht, Eq_ht
  | I31_ht, I31_ht
  | Struct_ht, Struct_ht
  | Array_ht, Array_ht
  | None_ht, None_ht
  | Func_ht, Func_ht
  | NoFunc_ht, NoFunc_ht
  | Exn_ht, Exn_ht
  | NoExn_ht, NoExn_ht
  | Extern_ht, Extern_ht
  | NoExtern_ht, NoExtern_ht ->
    0
  | TypeUse id1, TypeUse id2 -> compare_indice id1 id2
  | TypeUse _, _ -> 1
  | _, TypeUse _ -> -1
  | Any_ht, _ -> 1
  | _, Any_ht -> -1
  | Eq_ht, _ -> 1
  | _, Eq_ht -> -1
  | I31_ht, _ -> 1
  | _, I31_ht -> -1
  | Struct_ht, _ -> 1
  | _, Struct_ht -> -1
  | Array_ht, _ -> 1
  | _, Array_ht -> -1
  | None_ht, _ -> 1
  | _, None_ht -> -1
  | Func_ht, _ -> 1
  | _, Func_ht -> -1
  | NoFunc_ht, _ -> 1
  | _, NoFunc_ht -> -1
  | Exn_ht, _ -> 1
  | _, Exn_ht -> -1
  | NoExn_ht, _ -> 1
  | _, NoExn_ht -> -1
  | Extern_ht, _ -> 1
  | _, Extern_ht -> -1

type nonrec ref_type = nullable * heap_type

let pp_ref_type ppf (n, ht) =
  match n with
  | No_null -> pf ppf "(ref %a)" pp_heap_type ht
  | Null -> pf ppf "(ref null %a)" pp_heap_type ht

let ref_type_eq t1 t2 =
  match (t1, t2) with
  | (Null, t1), (Null, t2) | (No_null, t1), (No_null, t2) -> heap_type_eq t1 t2
  | _ -> false

let is_subtype_ref_type t1 t2 =
  match (t1, t2) with
  | (No_null, ht1), (Null, ht2) when heap_type_eq ht1 ht2 -> true
  | (No_null, TypeUse _), (Null, Func_ht)
  | (No_null, TypeUse _), (No_null, Func_ht)
  | (Null, TypeUse _), (Null, Func_ht) ->
    true
  | (Null, t1), (Null, t2) | (No_null, t1), (No_null, t2) -> heap_type_eq t1 t2
  | _ -> false

let compare_nullable n1 n2 =
  match (n1, n2) with
  | Null, Null | No_null, No_null -> 0
  | Null, No_null -> -1
  | No_null, Null -> 1

let compare_ref_type t1 t2 =
  match (t1, t2) with
  | (Null, t1), (Null, t2) | (No_null, t1), (No_null, t2) ->
    compare_heap_type t1 t2
  | (Null, _), (No_null, _) -> -1
  | (No_null, _), (Null, _) -> 1

type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

type pack_type =
  | I8
  | I16

type storage_type =
  | Val_type of val_type
  | Pack_type of pack_type

let pp_val_type fmt = function
  | Num_type t -> pp_num_type fmt t
  | Ref_type t -> pp_ref_type fmt t

let val_type_eq t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> num_type_eq t1 t2
  | Ref_type t1, Ref_type t2 -> ref_type_eq t1 t2
  | _, _ -> false

let is_subtype_val_type t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> num_type_eq t1 t2
  | Ref_type t1, Ref_type t2 -> is_subtype_ref_type t1 t2
  | _, _ -> false

let compare_val_type t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> compare_num_type t1 t2
  | Ref_type t1, Ref_type t2 -> compare_ref_type t1 t2
  | Num_type _, _ -> 1
  | Ref_type _, _ -> -1

type nonrec param = string option * val_type

let pp_param ppf (id, vt) = pf ppf "(param%a %a)" pp_id_opt id pp_val_type vt

let param_eq (_, t1) (_, t2) = val_type_eq t1 t2

let compare_param (_, t1) (_, t2) = compare_val_type t1 t2

type nonrec param_type = param list

let pp_param_type ppf params = list ~sep:sp pp_param ppf params

let param_type_eq t1 t2 = List.equal param_eq t1 t2

let compare_param_type t1 t2 = List.compare compare_param t1 t2

type nonrec result_type = val_type list

let pp_result_ ppf vt = pf ppf "(result %a)" pp_val_type vt

let pp_result_type ppf results = list ~sep:sp pp_result_ ppf results

let result_type_eq t1 t2 = List.equal val_type_eq t1 t2

let compare_result_type t1 t2 = List.compare compare_val_type t1 t2

(* wrap printer to print a space before a non empty list *)
(* TODO or make it an optional arg of pp_list? *)
let with_space_list printer ppf l =
  match l with [] -> () | _l -> pf ppf " %a" printer l

type nonrec func_type = param_type * result_type

let pp_func_type ppf (params, results) =
  pf ppf "(func%a%a)"
    (with_space_list pp_param_type)
    params
    (with_space_list pp_result_type)
    results

let func_type_eq (pt1, rt1) (pt2, rt2) =
  param_type_eq pt1 pt2 && result_type_eq rt1 rt2

type field_type = mut * storage_type

type field = indice option * field_type

type comp_type =
  | Def_struct_t of field list
  | Def_array_t of field_type
  | Def_func_t of func_type

type sub_type =
  { final : bool
  ; ids : indice list
  ; ct : comp_type
  }

type block_type =
  | Bt_ind of indice
  | Bt_raw of (indice option * func_type)

let pp_block_type ppf = function
  | Bt_ind ind -> pf ppf "(type %a)" pp_indice ind
  | Bt_raw (_ind, (pt, rt)) ->
    pf ppf "%a%a"
      (with_space_list pp_param_type)
      pt
      (with_space_list pp_result_type)
      rt

let pp_block_type_opt ppf = function
  | None -> ()
  | Some bt -> pp_block_type ppf bt

let compare_func_type (pt1, rt1) (pt2, rt2) =
  let pt = compare_param_type pt1 pt2 in
  if pt = 0 then compare_result_type rt1 rt2 else pt

(** I32 instructions *)

type i32_instr =
  | Const of Int32.t
  | Clz
  | Ctz
  | Popcnt
  | Add
  | Sub
  | Mul
  | Div of sx
  | Rem of sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx
  | Extend8_s
  | Extend16_s
  | Wrap_i64
  | Trunc_f of nn * sx
  | Trunc_sat_f of nn * sx
  | Reinterpret_f of nn
  | Load of indice * memarg
  | Load8 of indice * sx * memarg
  | Load16 of indice * sx * memarg
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
  | Div sx -> pf ppf "i32.div_%a" pp_sx sx
  | Rem sx -> pf ppf "i32.rem_%a" pp_sx sx
  | And -> pf ppf "i32.and"
  | Or -> pf ppf "i32.or"
  | Xor -> pf ppf "i32.xor"
  | Shl -> pf ppf "i32.shl"
  | Shr sx -> pf ppf "i32.shr_%a" pp_sx sx
  | Rotl -> pf ppf "i32.rotl"
  | Rotr -> pf ppf "i32.rotr"
  | Eqz -> pf ppf "i32.eqz"
  | Eq -> pf ppf "i32.eq"
  | Ne -> pf ppf "i32.ne"
  | Lt sx -> pf ppf "i32.lt_%a" pp_sx sx
  | Gt sx -> pf ppf "i32.gt_%a" pp_sx sx
  | Le sx -> pf ppf "i32.le_%a" pp_sx sx
  | Ge sx -> pf ppf "i32.ge_%a" pp_sx sx
  | Extend8_s -> pf ppf "i32.extend8_s"
  | Extend16_s -> pf ppf "i32.extend16_s"
  | Wrap_i64 -> pf ppf "i32.wrap_i64"
  | Trunc_f (nn, sx) -> pf ppf "i32.trunc_f%a_%a" pp_nn nn pp_sx sx
  | Trunc_sat_f (nn, sx) -> pf ppf "i32.truc_sat_f%a_%a" pp_nn nn pp_sx sx
  | Reinterpret_f nn -> pf ppf "i32.reinterpret_f%a" pp_nn nn
  | Load (indice, memarg) ->
    pf ppf "i32.load%a%a" pp_indice_not0 indice pp_memarg memarg
  | Load8 (indice, sx, memarg) ->
    pf ppf "i32.load8_%a%a%a" pp_sx sx pp_indice_not0 indice pp_memarg memarg
  | Load16 (indice, sx, memarg) ->
    pf ppf "i32.load16_%a%a%a" pp_sx sx pp_indice_not0 indice pp_memarg memarg
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
  | Div of sx
  | Rem of sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx
  | Extend8_s
  | Extend16_s
  | Extend32_s
  | Extend_i32 of sx
  | Trunc_f of nn * sx
  | Trunc_sat_f of nn * sx
  | Reinterpret_f of nn
  | Load of indice * memarg
  | Load8 of indice * sx * memarg
  | Load16 of indice * sx * memarg
  | Load32 of indice * sx * memarg
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
  | Div sx -> pf ppf "i64.div_%a" pp_sx sx
  | Rem sx -> pf ppf "i64.rem_%a" pp_sx sx
  | And -> pf ppf "i64.and"
  | Or -> pf ppf "i64.or"
  | Xor -> pf ppf "i64.xor"
  | Shl -> pf ppf "i64.shl"
  | Shr sx -> pf ppf "i64.shr_%a" pp_sx sx
  | Rotl -> pf ppf "i64.rotl"
  | Rotr -> pf ppf "i64.rotr"
  | Eqz -> pf ppf "i64.eqz"
  | Eq -> pf ppf "i64.eq"
  | Ne -> pf ppf "i64.ne"
  | Lt sx -> pf ppf "i64.lt_%a" pp_sx sx
  | Gt sx -> pf ppf "i64.gt_%a" pp_sx sx
  | Le sx -> pf ppf "i64.le_%a" pp_sx sx
  | Ge sx -> pf ppf "i64.ge_%a" pp_sx sx
  | Extend8_s -> pf ppf "i64.extend8_s"
  | Extend16_s -> pf ppf "i64.extend16_s"
  | Extend32_s -> pf ppf "i64.extend32_s"
  | Extend_i32 sx -> pf ppf "i64.extend_i32_%a" pp_sx sx
  | Trunc_f (nn, sx) -> pf ppf "i64.trunc_f%a_%a" pp_nn nn pp_sx sx
  | Trunc_sat_f (nn, sx) -> pf ppf "i64.trunc_sat_f%a_%a" pp_nn nn pp_sx sx
  | Reinterpret_f nn -> pf ppf "i64.reinterpret_f%a" pp_nn nn
  | Load (indice, memarg) ->
    pf ppf "i64.load%a%a" pp_indice_not0 indice pp_memarg memarg
  | Load8 (indice, sx, memarg) ->
    pf ppf "i64.load8_%a%a%a" pp_indice_not0 indice pp_sx sx pp_memarg memarg
  | Load16 (indice, sx, memarg) ->
    pf ppf "i64.load16_%a%a%a" pp_indice_not0 indice pp_sx sx pp_memarg memarg
  | Load32 (indice, sx, memarg) ->
    pf ppf "i64.load32_%a%a%a" pp_indice_not0 indice pp_sx sx pp_memarg memarg
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
  | Convert_i of nn * sx
  | Reinterpret_i of nn
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
  | Convert_i (nn, sx) -> pf ppf "f32.convert_i%a%a" pp_nn nn pp_sx sx
  | Reinterpret_i nn -> pf ppf "f32.reinterpret_i%a" pp_nn nn
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
  | Convert_i of nn * sx
  | Reinterpret_i of nn
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
  | Convert_i (nn, sx) -> pf ppf "f64.convert_i%a_%a" pp_nn nn pp_sx sx
  | Reinterpret_i nn -> pf ppf "f64.reinterpret_i%a" pp_nn nn
  | Load (indice, memarg) ->
    pf ppf "f64.load%a%a" pp_indice_not0 indice pp_memarg memarg
  | Store (indice, memarg) ->
    pf ppf "f64.store%a%a" pp_indice_not0 indice pp_memarg memarg

(** V128 instructions *)
type v128_instr = Const of Concrete_v128.t

let pp_v128_instr ppf = function
  | Const n -> pf ppf "v128.const %a" Concrete_v128.pp n

(** I8x16 instructions *)
type i8x16_instr =
  | Add
  | Sub

let pp_i8x16_instr ppf = function
  | Add -> pf ppf "i8x16.add"
  | Sub -> pf ppf "i8x16.sub"

(** I16x8 instructions *)
type i16x8_instr =
  | Add
  | Sub

let pp_i16x8_instr ppf = function
  | Add -> pf ppf "i16x8.add"
  | Sub -> pf ppf "i16x8.sub"

(* I32x4 instructions *)
type i32x4_instr =
  | Add
  | Sub

let pp_i32x4_instr ppf = function
  | Add -> pf ppf "i32x4.add"
  | Sub -> pf ppf "i32x4.sub"

(** I64x2 instructions *)
type i64x2_instr =
  | Add
  | Sub

let pp_i64x2_instr ppf = function
  | Add -> pf ppf "i64x2.add"
  | Sub -> pf ppf "i64x2.sub"

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
  | Copy (Raw 0, Raw 0) -> pf ppf "memory.copy"
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
  | V128 of v128_instr
  | I8x16 of i8x16_instr
  | I16x8 of i16x8_instr
  | I32x4 of i32x4_instr
  | I64x2 of i64x2_instr
  | Ref of ref_instr
  | Local of local_instr
  | Global of global_instr
  | Table of table_instr
  | Elem of elem_instr
  | Memory of memory_instr
  | Data of data_instr
  | Drop
  | Select of val_type list option
  | Nop
  | Unreachable
  | Block of string option * block_type option * expr
  | Loop of string option * block_type option * expr
  | If_else of string option * block_type option * expr * expr
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

and expr = instr list

let pp_newline ppf () = pf ppf "@\n"

let rec pp_instr ~short ppf = function
  | I32 i -> pp_i32_instr ppf i
  | I64 i -> pp_i64_instr ppf i
  | F32 i -> pp_f32_instr ppf i
  | F64 i -> pp_f64_instr ppf i
  | V128 i -> pp_v128_instr ppf i
  | I8x16 i -> pp_i8x16_instr ppf i
  | I16x8 i -> pp_i16x8_instr ppf i
  | I32x4 i -> pp_i32x4_instr ppf i
  | I64x2 i -> pp_i64x2_instr ppf i
  | Ref i -> pp_ref_instr ppf i
  | Local i -> pp_local_instr ppf i
  | Global i -> pp_global_instr ppf i
  | Table i -> pp_table_instr ppf i
  | Elem i -> pp_elem_instr ppf i
  | Memory i -> pp_memory_instr ppf i
  | Data i -> pp_data_instr ppf i
  | Drop -> pf ppf "drop"
  | Select vt ->
    begin match vt with
    | None -> pf ppf "select"
    | Some vt -> pf ppf "select (%a)" pp_result_type vt
    (* TODO: are the parens needed ? *)
    end
  | Nop -> pf ppf "nop"
  | Unreachable -> pf ppf "unreachable"
  | Block (id, bt, e) ->
    if short then pf ppf "block%a%a" pp_id_opt id pp_block_type_opt bt
    else
      pf ppf "(block%a%a@\n  @[<v>%a@])" pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | Loop (id, bt, e) ->
    if short then pf ppf "loop%a%a" pp_id_opt id pp_block_type_opt bt
    else
      pf ppf "(loop%a%a@\n  @[<v>%a@])" pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | If_else (id, bt, e1, e2) ->
    let pp_else ppf =
     (function
     | [] -> ()
     | e -> pf ppf "@\n(else@\n  @[<v>%a@]@\n)" (pp_expr ~short) e )
    in
    if short then pf ppf "if%a%a" pp_id_opt id pp_block_type_opt bt
    else
      pf ppf "(if%a%a@\n  @[<v>(then@\n  @[<v>%a@]@\n)%a@]@\n)" pp_id_opt id
        pp_block_type_opt bt (pp_expr ~short) e1 pp_else e2
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
  (* convesion *)
  | Any_convert_extern -> pf fmt "any.convert_extern"
  | Extern_convert_any -> pf fmt "extern.convert_any"

and pp_expr ~short ppf instrs =
  list ~sep:pp_newline (fun ppf i -> (pp_instr ~short ppf) i) ppf instrs

module Func = struct
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr
    ; id : string option
    }

  let pp_local ppf (id, t) = pf ppf "(local%a %a)" pp_id_opt id pp_val_type t

  let pp_locals ppf locals = list ~sep:sp pp_local ppf locals

  let pp ppf f =
    (* TODO: type_use ? *)
    pf ppf "(func%a%a%a@\n  @[<v>%a@]@\n)" pp_id_opt f.id pp_block_type f.type_f
      (with_space_list pp_locals)
      f.locals (pp_expr ~short:false) f.body
end

module Typedef = struct
  type t = string option * sub_type

  let pp _fmt (_id, _t) = assert false
  (* pf fmt "(type%a %a)" pp_id_opt id pp_func_type t *)
end

module Table = struct
  module Type = struct
    type nonrec t = limits * ref_type

    let pp ppf (limits, ref_type) =
      pf ppf "%a %a" pp_limits limits pp_ref_type ref_type
  end

  type t =
    { id : string option
    ; typ : Type.t
    ; init : expr option
    }

  let pp ppf { id; typ; init } =
    pf ppf "(table%a %a%a)" pp_id_opt id Type.pp typ
      (fun ppf e ->
        match e with
        | None -> ()
        | Some e -> Fmt.pf ppf " %a" (pp_expr ~short:true) e )
      init
end

module Global = struct
  module Type = struct
    type nonrec t = mut * val_type

    let pp ppf (mut, val_type) =
      match mut with
      | Var -> pf ppf "(mut %a)" pp_val_type val_type
      | Const -> pf ppf "%a" pp_val_type val_type
  end

  type t =
    { typ : Type.t
    ; init : expr
    ; id : string option
    }

  let pp ppf (g : t) =
    pf ppf "(global%a %a %a)" pp_id_opt g.id Type.pp g.typ
      (pp_expr ~short:false) g.init
end

module Data = struct
  module Mode = struct
    type t =
      | Passive
      | Active of indice option * expr

    let pp ppf = function
      | Passive -> ()
      | Active (i, e) ->
        pf ppf "(memory %a) (offset %a)" pp_indice_opt i (pp_expr ~short:false)
          e
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }

  let pp ppf (d : t) =
    pf ppf {|(data%a %a %S)|} pp_id_opt d.id Mode.pp d.mode d.init
end

module Tag = struct
  type t =
    { id : string option
    ; typ : block_type
    }

  let pp ppf { id; typ } = pf ppf {|(tag%a %a)|} pp_id_opt id pp_block_type typ
end

module Elem = struct
  module Mode = struct
    type t =
      | Passive
      | Declarative
      | Active of indice option * expr

    let pp ppf = function
      | Passive -> ()
      | Declarative -> pf ppf "declare"
      | Active (i, e) -> (
        match i with
        | None -> pf ppf "(offset %a)" (pp_expr ~short:false) e
        | Some i ->
          pf ppf "(table %a) (offset %a)" pp_indice i (pp_expr ~short:false) e )
  end

  type t =
    { id : string option
    ; typ : ref_type
    ; init : expr list
    ; mode : Mode.t
    ; explicit_typ : bool
    }

  let pp_items ppf e = pf ppf "(item %a)" (pp_expr ~short:false) e

  let pp ppf (e : t) =
    pf ppf "@[<hov 2>(elem%a %a %a %a)@]" pp_id_opt e.id Mode.pp e.mode
      pp_ref_type e.typ
      (list ~sep:pp_newline pp_items)
      e.init
end

module Mem = struct
  type nonrec t = string option * limits

  let pp ppf (id, ty) = pf ppf "(memory%a %a)" pp_id_opt id pp_limits ty
end

module Import = struct
  module Type = struct
    type t =
      | Func of string option * block_type
      | Table of string option * Table.Type.t
      | Mem of string option * limits
      | Global of string option * Global.Type.t
      | Tag of string option * block_type

    let pp ppf = function
      | Func (id, t) -> pf ppf "(func%a %a)" pp_id_opt id pp_block_type t
      | Table (id, t) -> pf ppf "(table%a %a)" pp_id_opt id Table.Type.pp t
      | Mem (id, t) -> pf ppf "(memory%a %a)" pp_id_opt id pp_limits t
      | Global (id, t) -> pf ppf "(global%a %a)" pp_id_opt id Global.Type.pp t
      | Tag (id, t) -> pf ppf "(tag%a %a)" pp_id_opt id pp_block_type t
  end

  type t =
    { modul_name : string
        (** The name of the module from which the import is done *)
    ; name : string  (** The name of the importee in its module of origin *)
    ; typ : Type.t
    }

  let pp ppf i =
    pf ppf {|(import "%a" "%a" %a)|} string i.modul_name string i.name Type.pp
      i.typ
end

module Export = struct
  module Type = struct
    type t =
      | Func of indice option
      | Table of indice option
      | Mem of indice option
      | Global of indice option
      | Tag of indice option

    let pp ppf = function
      | Func id -> pf ppf "(func %a)" pp_indice_opt id
      | Table id -> pf ppf "(table %a)" pp_indice_opt id
      | Mem id -> pf ppf "(memory %a)" pp_indice_opt id
      | Global id -> pf ppf "(global %a)" pp_indice_opt id
      | Tag id -> pf ppf "(tag %a)" pp_indice_opt id
  end

  type t =
    { name : string
    ; typ : Type.t
    }

  let pp ppf (e : t) = pf ppf {|(export "%s" %a)|} e.name Type.pp e.typ
end

module Module = struct
  module Field = struct
    type t =
      | Typedef of Typedef.t
      | Global of Global.t
      | Table of Table.t
      | Mem of Mem.t
      | Func of Func.t
      | Elem of Elem.t
      | Data of Data.t
      | Tag of Tag.t
      | Start of indice
      | Import of Import.t
      | Export of Export.t

    let pp_start ppf start = pf ppf "(start %a)" pp_indice start

    let pp ppf = function
      | Typedef t -> Typedef.pp ppf t
      | Global g -> Global.pp ppf g
      | Table t -> Table.pp ppf t
      | Mem m -> Mem.pp ppf m
      | Func f -> Func.pp ppf f
      | Elem e -> Elem.pp ppf e
      | Data d -> Data.pp ppf d
      | Tag t -> Tag.pp ppf t
      | Start s -> pp_start ppf s
      | Import i -> Import.pp ppf i
      | Export e -> Export.pp ppf e
  end

  type t =
    { id : string option
    ; fields : Field.t list
    }

  let pp_fields ppf fields =
    Fmt.pf ppf "%a" (list ~sep:pp_newline Field.pp) fields

  let pp ppf m =
    pf ppf "(module%a@\n  @[<v>%a@]@\n)" pp_id_opt m.id pp_fields m.fields
end
