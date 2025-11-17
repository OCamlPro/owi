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

let pp_id fmt id = pf fmt "$%s" id

let pp_id_opt fmt = function None -> () | Some i -> pf fmt " %a" pp_id i

let pp_indice fmt = function Raw u -> int fmt u | Text i -> pp_id fmt i

let compare_indice id1 id2 =
  match (id1, id2) with
  | Raw i1, Raw i2 -> compare i1 i2
  | Text s1, Text s2 -> String.compare s1 s2
  | Raw _, Text _ -> -1
  | Text _, Raw _ -> 1

let indice_eq id1 id2 = compare_indice id1 id2 = 0

let pp_indice_opt fmt = function None -> () | Some i -> pp_indice fmt i

let pp_indices fmt ids = list ~sep:sp pp_indice fmt ids

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64
  | V128

let pp_num_type fmt = function
  | I32 -> pf fmt "i32"
  | I64 -> pf fmt "i64"
  | F32 -> pf fmt "f32"
  | F64 -> pf fmt "f64"
  | V128 -> pf fmt "v128"

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

let pp_nullable fmt = function
  | No_null ->
    (* TODO: no notation to enforce nonnull ? *)
    pf fmt ""
  | Null -> pf fmt "null"

type nonrec mut =
  | Const
  | Var

let pp_mut fmt = function Const -> () | Var -> pf fmt "mut"

let is_mut = function Const -> false | Var -> true

type nonrec nn =
  | S32
  | S64

let pp_nn fmt = function S32 -> pf fmt "32" | S64 -> pf fmt "64"

type nonrec ishape =
  | I8x16
  | I16x8
  | I32x4
  | I64x2

let pp_ishape fmt = function
  | I8x16 -> pf fmt "i8x16"
  | I16x8 -> pf fmt "i16x8"
  | I32x4 -> pf fmt "i32x4"
  | I64x2 -> pf fmt "i64x2"

type nonrec fshape =
  | F32x4
  | F64x8

let pp_fshape fmt = function F32x4 -> pf fmt "f32x4" | F64x8 -> pf fmt "f64x8"

type nonrec sx =
  | U
  | S

let pp_sx fmt = function U -> pf fmt "u" | S -> pf fmt "s"

type nonrec iunop =
  | Clz
  | Ctz
  | Popcnt

let pp_iunop fmt = function
  | Clz -> pf fmt "clz"
  | Ctz -> pf fmt "ctz"
  | Popcnt -> pf fmt "popcnt"

type nonrec funop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

let pp_funop fmt = function
  | Abs -> pf fmt "abs"
  | Neg -> pf fmt "neg"
  | Sqrt -> pf fmt "sqrt"
  | Ceil -> pf fmt "ceil"
  | Floor -> pf fmt "floor"
  | Trunc -> pf fmt "trunc"
  | Nearest -> pf fmt "nearest"

type nonrec vibinop =
  | Add
  | Sub

let pp_vibinop fmt = function Add -> pf fmt "add" | Sub -> pf fmt "sub"

type nonrec ibinop =
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

let pp_ibinop fmt = function
  | (Add : ibinop) -> pf fmt "add"
  | Sub -> pf fmt "sub"
  | Mul -> pf fmt "mul"
  | Div s -> pf fmt "div_%a" pp_sx s
  | Rem s -> pf fmt "rem_%a" pp_sx s
  | And -> pf fmt "and"
  | Or -> pf fmt "or"
  | Xor -> pf fmt "xor"
  | Shl -> pf fmt "shl"
  | Shr s -> pf fmt "shr_%a" pp_sx s
  | Rotl -> pf fmt "rotl"
  | Rotr -> pf fmt "rotr"

type nonrec fbinop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

let pp_fbinop fmt = function
  | (Add : fbinop) -> pf fmt "add"
  | Sub -> pf fmt "sub"
  | Mul -> pf fmt "mul"
  | Div -> pf fmt "div"
  | Min -> pf fmt "min"
  | Max -> pf fmt "max"
  | Copysign -> pf fmt "copysign"

(* TODO: inline this *)
type nonrec itestop = Eqz

let pp_itestop fmt = function Eqz -> pf fmt "eqz"

type nonrec irelop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

let pp_irelop fmt : irelop -> Unit.t = function
  | Eq -> pf fmt "eq"
  | Ne -> pf fmt "ne"
  | Lt sx -> pf fmt "lt_%a" pp_sx sx
  | Gt sx -> pf fmt "gt_%a" pp_sx sx
  | Le sx -> pf fmt "le_%a" pp_sx sx
  | Ge sx -> pf fmt "ge_%a" pp_sx sx

type nonrec frelop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

let frelop fmt : frelop -> Unit.t = function
  | Eq -> pf fmt "eq"
  | Ne -> pf fmt "ne"
  | Lt -> pf fmt "lt"
  | Gt -> pf fmt "gt"
  | Le -> pf fmt "le"
  | Ge -> pf fmt "ge"

type nonrec memarg =
  { offset : Int32.t
  ; align : Int32.t
  }

let pp_memarg =
  let pow_2 n =
    assert (Int32.ge n 0l);
    Int32.shl 1l n
  in
  fun fmt { offset; align } ->
    let pp_offset fmt offset =
      if Int32.gt offset 0l then pf fmt "offset=%ld " offset
    in
    pf fmt "%aalign=%ld" pp_offset offset (pow_2 align)

type nonrec limits =
  { min : int
  ; max : int option
  }

let pp_limits fmt { min; max } =
  match max with None -> pf fmt "%d" min | Some max -> pf fmt "%d %d" min max

type nonrec mem = string option * limits

let pp_mem fmt (id, ty) = pf fmt "(memory%a %a)" pp_id_opt id pp_limits ty

(** Structure *)

(** Types *)

type heap_type =
  | Func_ht
  | Extern_ht

let pp_heap_type fmt = function
  | Func_ht -> pf fmt "func"
  | Extern_ht -> pf fmt "extern"

let pp_heap_type_short fmt = function
  | Func_ht -> pf fmt "funcref"
  | Extern_ht -> pf fmt "externref"

let heap_type_eq t1 t2 =
  (* TODO: this is wrong *)
  match (t1, t2) with
  | Func_ht, Func_ht | Extern_ht, Extern_ht -> true
  | _, _ -> false

let compare_heap_type t1 t2 =
  (* TODO: this is wrong *)
  let to_int = function Func_ht -> 0 | Extern_ht -> 1 in
  Int.compare (to_int t1) (to_int t2)

type nonrec ref_type = nullable * heap_type

let pp_ref_type fmt (n, ht) =
  match n with
  | No_null -> pf fmt "%a" pp_heap_type_short ht
  | Null -> pf fmt "(ref null %a)" pp_heap_type ht

let ref_type_eq t1 t2 =
  match (t1, t2) with
  | (Null, t1), (Null, t2) | (No_null, t1), (No_null, t2) -> heap_type_eq t1 t2
  | _ -> false

let compare_ref_type t1 t2 =
  match (t1, t2) with
  | (Null, t1), (Null, t2) | (No_null, t1), (No_null, t2) ->
    compare_heap_type t1 t2
  | (Null, _), (No_null, _) -> -1
  | (No_null, _), (Null, _) -> 1

type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

let pp_val_type fmt = function
  | Num_type t -> pp_num_type fmt t
  | Ref_type t -> pp_ref_type fmt t

let val_type_eq t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> num_type_eq t1 t2
  | Ref_type t1, Ref_type t2 -> ref_type_eq t1 t2
  | _, _ -> false

let compare_val_type t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> compare_num_type t1 t2
  | Ref_type t1, Ref_type t2 -> compare_ref_type t1 t2
  | Num_type _, _ -> 1
  | Ref_type _, _ -> -1

type nonrec param = string option * val_type

let pp_param fmt (id, vt) = pf fmt "(param%a %a)" pp_id_opt id pp_val_type vt

let param_eq (_, t1) (_, t2) = val_type_eq t1 t2

let compare_param (_, t1) (_, t2) = compare_val_type t1 t2

type nonrec param_type = param list

let pp_param_type fmt params = list ~sep:sp pp_param fmt params

let param_type_eq t1 t2 = List.equal param_eq t1 t2

let compare_param_type t1 t2 = List.compare compare_param t1 t2

type nonrec result_type = val_type list

let pp_result_ fmt vt = pf fmt "(result %a)" pp_val_type vt

let pp_result_type fmt results = list ~sep:sp pp_result_ fmt results

let result_type_eq t1 t2 = List.equal val_type_eq t1 t2

let compare_result_type t1 t2 = List.compare compare_val_type t1 t2

(* wrap printer to print a space before a non empty list *)
(* TODO or make it an optional arg of pp_list? *)
let with_space_list printer fmt l =
  match l with [] -> () | _l -> pf fmt " %a" printer l

type nonrec func_type = param_type * result_type

let pp_func_type fmt (params, results) =
  pf fmt "(func%a%a)"
    (with_space_list pp_param_type)
    params
    (with_space_list pp_result_type)
    results

let func_type_eq (pt1, rt1) (pt2, rt2) =
  param_type_eq pt1 pt2 && result_type_eq rt1 rt2

type block_type =
  | Bt_ind of indice
  | Bt_raw of (indice option * func_type)

let pp_block_type fmt = function
  | Bt_ind ind -> pf fmt "(type %a)" pp_indice ind
  | Bt_raw (_ind, (pt, rt)) ->
    pf fmt "%a%a"
      (with_space_list pp_param_type)
      pt
      (with_space_list pp_result_type)
      rt

let pp_block_type_opt fmt = function
  | None -> ()
  | Some bt -> pp_block_type fmt bt

let compare_func_type (pt1, rt1) (pt2, rt2) =
  let pt = compare_param_type pt1 pt2 in
  if pt = 0 then compare_result_type rt1 rt2 else pt

type nonrec table_type = limits * ref_type

let pp_table_type fmt (limits, ref_type) =
  pf fmt "%a %a" pp_limits limits pp_ref_type ref_type

type nonrec global_type = mut * val_type

let pp_global_type fmt (mut, val_type) =
  match mut with
  | Var -> pf fmt "(mut %a)" pp_val_type val_type
  | Const -> pf fmt "%a" pp_val_type val_type

type nonrec extern_type =
  | Func of string option * func_type
  | Table of string option * table_type
  | Mem of string option * limits
  | Global of string option * global_type

(** Instructions *)

type instr =
  (* Numeric Instructions *)
  | I32_const of Int32.t
  | I64_const of Int64.t
  | F32_const of Float32.t
  | F64_const of Float64.t
  | V128_const of V128.t
  | I_unop of nn * iunop
  | F_unop of nn * funop
  | I_binop of nn * ibinop
  | F_binop of nn * fbinop
  | V_ibinop of ishape * vibinop
  | I_testop of nn * itestop
  | I_relop of nn * irelop
  | F_relop of nn * frelop
  | I_extend8_s of nn
  | I_extend16_s of nn
  | I64_extend32_s
  | I32_wrap_i64
  | I64_extend_i32 of sx
  | I_trunc_f of nn * nn * sx
  | I_trunc_sat_f of nn * nn * sx
  | F32_demote_f64
  | F64_promote_f32
  | F_convert_i of nn * nn * sx
  | I_reinterpret_f of nn * nn
  | F_reinterpret_i of nn * nn
  (* Reference instructions *)
  | Ref_null of heap_type
  | Ref_is_null
  | Ref_func of indice
  (* Parametric instructions *)
  | Drop
  | Select of val_type list option
  (* Variable instructions *)
  | Local_get of indice
  | Local_set of indice
  | Local_tee of indice
  | Global_get of indice
  | Global_set of indice
  (* Table instructions *)
  | Table_get of indice
  | Table_set of indice
  | Table_size of indice
  | Table_grow of indice
  | Table_fill of indice
  | Table_copy of indice * indice
  | Table_init of indice * indice
  | Elem_drop of indice
  (* Memory instructions *)
  | I_load of nn * memarg
  | F_load of nn * memarg
  | I_store of nn * memarg
  | F_store of nn * memarg
  | I_load8 of nn * sx * memarg
  | I_load16 of nn * sx * memarg
  | I64_load32 of sx * memarg
  | I_store8 of nn * memarg
  | I_store16 of nn * memarg
  | I64_store32 of memarg
  | Memory_size
  | Memory_grow
  | Memory_fill
  | Memory_copy
  | Memory_init of indice
  | Data_drop of indice
  (* Control instructions *)
  | Nop
  | Unreachable
  | Block of string option * block_type option * expr Annotated.t
  | Loop of string option * block_type option * expr Annotated.t
  | If_else of
      string option * block_type option * expr Annotated.t * expr Annotated.t
  | Br of indice
  | Br_if of indice
  | Br_table of indice array * indice
  | Return
  | Return_call of indice
  | Return_call_indirect of indice * block_type
  | Return_call_ref of block_type
  | Call of indice
  | Call_indirect of indice * block_type
  | Call_ref of indice
  (* extern *)
  | Extern_externalize
  | Extern_internalize

and expr = instr Annotated.t list

let pp_newline ppf () = pf ppf "@\n"

let rec pp_instr ~short fmt = function
  | I32_const i -> pf fmt "i32.const %ld" i
  | I64_const i -> pf fmt "i64.const %Ld" i
  | F32_const f -> pf fmt "f32.const %a" Float32.pp f
  | F64_const f -> pf fmt "f64.const %a" Float64.pp f
  | V128_const f -> pf fmt "v128.const %a" V128.pp f
  | I_unop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_iunop op
  | F_unop (n, op) -> pf fmt "f%a.%a" pp_nn n pp_funop op
  | I_binop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_ibinop op
  | F_binop (n, op) -> pf fmt "f%a.%a" pp_nn n pp_fbinop op
  | V_ibinop (shape, op) -> pf fmt "%a.%a" pp_ishape shape pp_vibinop op
  | I_testop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_itestop op
  | I_relop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_irelop op
  | F_relop (n, op) -> pf fmt "f%a.%a" pp_nn n frelop op
  | I_extend8_s n -> pf fmt "i%a.extend8_s" pp_nn n
  | I_extend16_s n -> pf fmt "i%a.extend16_s" pp_nn n
  | I64_extend32_s -> pf fmt "i64.extend32_s"
  | I32_wrap_i64 -> pf fmt "i32.wrap_i64"
  | I64_extend_i32 sx -> pf fmt "i64.extend_i32_%a" pp_sx sx
  | I_trunc_f (n, n', sx) -> pf fmt "i%a.trunc_f%a_%a" pp_nn n pp_nn n' pp_sx sx
  | I_trunc_sat_f (n, n', sx) ->
    pf fmt "i%a.trunc_sat_f%a_%a" pp_nn n pp_nn n' pp_sx sx
  | F32_demote_f64 -> pf fmt "f32.demote_f64"
  | F64_promote_f32 -> pf fmt "f64.promote_f32"
  | F_convert_i (n, n', sx) ->
    pf fmt "f%a.convert_i%a_%a" pp_nn n pp_nn n' pp_sx sx
  | I_reinterpret_f (n, n') -> pf fmt "i%a.reinterpret_f%a" pp_nn n pp_nn n'
  | F_reinterpret_i (n, n') -> pf fmt "f%a.reinterpret_i%a" pp_nn n pp_nn n'
  | Ref_null t -> pf fmt "ref.null %a" pp_heap_type t
  | Ref_is_null -> pf fmt "ref.is_null"
  | Ref_func fid -> pf fmt "ref.func %a" pp_indice fid
  | Drop -> pf fmt "drop"
  | Select vt -> begin
    match vt with
    | None -> pf fmt "select"
    | Some vt -> pf fmt "select (%a)" pp_result_type vt
    (* TODO: are the parens needed ? *)
  end
  | Local_get id -> pf fmt "local.get %a" pp_indice id
  | Local_set id -> pf fmt "local.set %a" pp_indice id
  | Local_tee id -> pf fmt "local.tee %a" pp_indice id
  | Global_get id -> pf fmt "global.get %a" pp_indice id
  | Global_set id -> pf fmt "global.set %a" pp_indice id
  | Table_get id -> pf fmt "table.get %a" pp_indice id
  | Table_set id -> pf fmt "table.set %a" pp_indice id
  | Table_size id -> pf fmt "table.size %a" pp_indice id
  | Table_grow id -> pf fmt "table.grow %a" pp_indice id
  | Table_fill id -> pf fmt "table.fill %a" pp_indice id
  | Table_copy (id, id') -> pf fmt "table.copy %a %a" pp_indice id pp_indice id'
  | Table_init (tid, eid) ->
    pf fmt "table.init %a %a" pp_indice tid pp_indice eid
  | Elem_drop id -> pf fmt "elem.drop %a" pp_indice id
  | I_load (n, memarg) -> pf fmt "i%a.load %a" pp_nn n pp_memarg memarg
  | F_load (n, memarg) -> pf fmt "f%a.load %a" pp_nn n pp_memarg memarg
  | I_store (n, memarg) -> pf fmt "i%a.store %a" pp_nn n pp_memarg memarg
  | F_store (n, memarg) -> pf fmt "f%a.store %a" pp_nn n pp_memarg memarg
  | I_load8 (n, sx, memarg) ->
    pf fmt "i%a.load8_%a %a" pp_nn n pp_sx sx pp_memarg memarg
  | I_load16 (n, sx, memarg) ->
    pf fmt "i%a.load16_%a %a" pp_nn n pp_sx sx pp_memarg memarg
  | I64_load32 (sx, memarg) ->
    pf fmt "i64.load32_%a %a" pp_sx sx pp_memarg memarg
  | I_store8 (n, memarg) -> pf fmt "i%a.store8 %a" pp_nn n pp_memarg memarg
  | I_store16 (n, memarg) -> pf fmt "i%a.store16 %a" pp_nn n pp_memarg memarg
  | I64_store32 memarg -> pf fmt "i64.store32 %a" pp_memarg memarg
  | Memory_size -> pf fmt "memory.size"
  | Memory_grow -> pf fmt "memory.grow"
  | Memory_fill -> pf fmt "memory.fill"
  | Memory_copy -> pf fmt "memory.copy"
  | Memory_init id -> pf fmt "memory.init %a" pp_indice id
  | Data_drop id -> pf fmt "data.drop %a" pp_indice id
  | Nop -> pf fmt "nop"
  | Unreachable -> pf fmt "unreachable"
  | Block (id, bt, e) ->
    if short then pf fmt "block%a%a" pp_id_opt id pp_block_type_opt bt
    else
      pf fmt "(block%a%a@\n  @[<v>%a@])" pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | Loop (id, bt, e) ->
    if short then pf fmt "loop%a%a" pp_id_opt id pp_block_type_opt bt
    else
      pf fmt "(loop%a%a@\n  @[<v>%a@])" pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | If_else (id, bt, e1, e2) ->
    let pp_else fmt e =
      Annotated.iter
        (function
          | [] -> ()
          | _ -> pf fmt "@\n(else@\n  @[<v>%a@]@\n)" (pp_expr ~short) e )
        e
    in
    if short then pf fmt "if%a%a" pp_id_opt id pp_block_type_opt bt
    else
      pf fmt "(if%a%a@\n  @[<v>(then@\n  @[<v>%a@]@\n)%a@]@\n)" pp_id_opt id
        pp_block_type_opt bt (pp_expr ~short) e1 pp_else e2
  | Br id -> pf fmt "br %a" pp_indice id
  | Br_if id -> pf fmt "br_if %a" pp_indice id
  | Br_table (ids, id) ->
    pf fmt "br_table %a %a" (array ~sep:sp pp_indice) ids pp_indice id
  | Return -> pf fmt "return"
  | Return_call id -> pf fmt "return_call %a" pp_indice id
  | Return_call_indirect (tbl_id, ty_id) ->
    pf fmt "return_call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
  | Return_call_ref ty_id -> pf fmt "return_call_ref %a" pp_block_type ty_id
  | Call id -> pf fmt "call %a" pp_indice id
  | Call_indirect (tbl_id, ty_id) ->
    pf fmt "call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
  | Call_ref ty_id -> pf fmt "call_ref %a" pp_indice ty_id
  | Extern_externalize -> pf fmt "extern.externalize"
  | Extern_internalize -> pf fmt "extern.internalize"

and pp_expr ~short fmt instrs =
  Annotated.iter
    (fun instrs ->
      list ~sep:pp_newline
        (fun fmt i -> Annotated.iter (pp_instr ~short fmt) i)
        fmt instrs )
    instrs

let rec iter_expr f (e : expr Annotated.t) =
  Annotated.iter (List.iter (iter_instr f)) e

and iter_instr f instr =
  Annotated.iter f instr;
  Annotated.iter
    (function
      | I32_const _ | I64_const _ | F32_const _ | F64_const _ | V128_const _
      | I_unop (_, _)
      | F_unop (_, _)
      | I_binop (_, _)
      | F_binop (_, _)
      | V_ibinop (_, _)
      | I_testop (_, _)
      | I_relop (_, _)
      | F_relop (_, _)
      | I_extend8_s _ | I_extend16_s _ | I64_extend32_s | I32_wrap_i64
      | I64_extend_i32 _
      | I_trunc_f (_, _, _)
      | I_trunc_sat_f (_, _, _)
      | F32_demote_f64 | F64_promote_f32
      | F_convert_i (_, _, _)
      | I_reinterpret_f (_, _)
      | F_reinterpret_i (_, _)
      | Ref_null _ | Ref_is_null | Ref_func _ | Drop | Select _ | Local_get _
      | Local_set _ | Local_tee _ | Global_get _ | Global_set _ | Table_get _
      | Table_set _ | Table_size _ | Table_grow _ | Table_fill _
      | Table_copy (_, _)
      | Table_init (_, _)
      | Elem_drop _
      | I_load (_, _)
      | F_load (_, _)
      | I_store (_, _)
      | F_store (_, _)
      | I_load8 (_, _, _)
      | I_load16 (_, _, _)
      | I64_load32 (_, _)
      | I_store8 (_, _)
      | I_store16 (_, _)
      | I64_store32 _ | Memory_size | Memory_grow | Memory_fill | Memory_copy
      | Memory_init _ | Data_drop _ | Nop | Unreachable | Br _ | Br_if _
      | Br_table (_, _)
      | Return | Return_call _
      | Return_call_indirect (_, _)
      | Return_call_ref _ | Call _
      | Call_indirect (_, _)
      | Call_ref _ | Extern_externalize | Extern_internalize ->
        ()
      | Block (_, _, e) | Loop (_, _, e) -> iter_expr f e
      | If_else (_, _, e1, e2) ->
        iter_expr f e1;
        iter_expr f e2 )
    instr

(* TODO: func and expr should also be parametrised on block type:
   using (param_type, result_type) M.block_type before simplify and directly an indice after *)
type func =
  { type_f : block_type
  ; locals : param list
  ; body : expr Annotated.t
  ; id : string option
  }

let pp_local fmt (id, t) = pf fmt "(local%a %a)" pp_id_opt id pp_val_type t

let pp_locals fmt locals = list ~sep:sp pp_local fmt locals

let pp_func fmt f =
  (* TODO: typeuse ? *)
  pf fmt "(func%a%a%a@\n  @[<v>%a@]@\n)" pp_id_opt f.id pp_block_type f.type_f
    (with_space_list pp_locals)
    f.locals (pp_expr ~short:false) f.body

let pp_funcs fmt (funcs : func list) = list ~sep:pp_newline pp_func fmt funcs

(* Tables & Memories *)

type table = string option * table_type

let pp_table fmt (id, ty) = pf fmt "(table%a %a)" pp_id_opt id pp_table_type ty

(* Modules *)

type import_desc =
  | Import_func of string option * block_type
  | Import_table of string option * table_type
  | Import_mem of string option * limits
  | Import_global of string option * global_type

let import_desc fmt = function
  | Import_func (id, t) -> pf fmt "(func%a %a)" pp_id_opt id pp_block_type t
  | Import_table (id, t) -> pf fmt "(table%a %a)" pp_id_opt id pp_table_type t
  | Import_mem (id, t) -> pf fmt "(memory%a %a)" pp_id_opt id pp_limits t
  | Import_global (id, t) ->
    pf fmt "(global%a %a)" pp_id_opt id pp_global_type t

type import =
  { modul : string  (** The name of the module from which the import is done *)
  ; name : string  (** The name of the importee in its module of origin *)
  ; desc : import_desc
      (** If this import_desc first field is Some s, the importee is made
          available under name s, else it can only be used via its numerical
          index.*)
  }

let pp_import fmt i =
  pf fmt {|(import "%a" "%a" %a)|} string i.modul string i.name import_desc
    i.desc

type export_desc =
  | Export_func of indice option
  | Export_table of indice option
  | Export_mem of indice option
  | Export_global of indice option

let pp_export_desc fmt = function
  | Export_func id -> pf fmt "(func %a)" pp_indice_opt id
  | Export_table id -> pf fmt "(table %a)" pp_indice_opt id
  | Export_mem id -> pf fmt "(memory %a)" pp_indice_opt id
  | Export_global id -> pf fmt "(global %a)" pp_indice_opt id

type export =
  { name : string
  ; desc : export_desc
  }

let pp_export fmt (e : export) =
  pf fmt {|(export "%s" %a)|} e.name pp_export_desc e.desc

type type_def = string option * func_type

let pp_type_def fmt (id, t) = pf fmt "(type%a %a)" pp_id_opt id pp_func_type t

let type_def_eq (id1, t1) (id2, t2) =
  Option.equal String.equal id1 id2 && func_type_eq t1 t2

let pp_start fmt start = pf fmt "(start %a)" pp_indice start

type global =
  { typ : global_type
  ; init : expr Annotated.t
  ; id : string option
  }

let pp_global fmt (g : global) =
  pf fmt "(global%a %a %a)" pp_id_opt g.id pp_global_type g.typ
    (pp_expr ~short:false) g.init

type data_mode =
  | Data_passive
  | Data_active of indice option * expr Annotated.t

let pp_data_mode fmt = function
  | Data_passive -> ()
  | Data_active (i, e) ->
    pf fmt "(memory %a) (offset %a)" pp_indice_opt i (pp_expr ~short:false) e

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

let pp_data fmt (d : data) =
  pf fmt {|(data%a %a %S)|} pp_id_opt d.id pp_data_mode d.mode d.init

type elem_mode =
  | Elem_passive
  | Elem_active of indice option * expr Annotated.t
  | Elem_declarative

let pp_elem_mode fmt = function
  | Elem_passive -> ()
  | Elem_declarative -> pf fmt "declare"
  | Elem_active (i, e) -> (
    match i with
    | None -> pf fmt "(offset %a)" (pp_expr ~short:false) e
    | Some i ->
      pf fmt "(table %a) (offset %a)" pp_indice i (pp_expr ~short:false) e )

type elem =
  { id : string option
  ; typ : ref_type
  ; init : expr Annotated.t list
  ; mode : elem_mode
  }

let pp_elem_expr fmt e = pf fmt "(item %a)" (pp_expr ~short:false) e

let pp_elem fmt (e : elem) =
  pf fmt "@[<hov 2>(elem%a %a %a %a)@]" pp_id_opt e.id pp_elem_mode e.mode
    pp_ref_type e.typ
    (list ~sep:pp_newline pp_elem_expr)
    e.init

type module_field =
  | MType of type_def
  | MGlobal of global
  | MTable of table
  | MMem of mem
  | MFunc of func
  | MElem of elem
  | MData of data
  | MStart of indice
  | MImport of import
  | MExport of export

let pp_module_field fmt = function
  | MType t -> pp_type_def fmt t
  | MGlobal g -> pp_global fmt g
  | MTable t -> pp_table fmt t
  | MMem m -> pp_mem fmt m
  | MFunc f -> pp_func fmt f
  | MElem e -> pp_elem fmt e
  | MData d -> pp_data fmt d
  | MStart s -> pp_start fmt s
  | MImport i -> pp_import fmt i
  | MExport e -> pp_export fmt e

type modul =
  { id : string option
  ; fields : module_field list
  }

let pp_modul fmt (m : modul) =
  pf fmt "(module%a@\n  @[<v>%a@]@\n)" pp_id_opt m.id
    (list ~sep:pp_newline pp_module_field)
    m.fields
