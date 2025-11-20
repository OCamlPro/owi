(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

let sp ppf () = Fmt.char ppf ' '

(* identifiers *)

type indice = int

let pp_id fmt id = pf fmt "$%s" id

let pp_id_opt fmt = function None -> () | Some i -> pf fmt " %a" pp_id i

let pp_indice fmt i = int fmt i

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

type nonrec mut =
  | Const
  | Var

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

let func_type_eq (pt1, rt1) (pt2, rt2) =
  param_type_eq pt1 pt2 && result_type_eq rt1 rt2

type block_type =
  (* TODO: inline this *)
  | Bt_raw of (indice option * func_type)

let pp_block_type fmt = function
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

module Func = struct
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }
end

(* Tables & Memories *)

module Table = struct
  module Type = struct
    type nonrec t = limits * ref_type
  end

  type t = string option * Type.t
end

(* Modules *)

module Typedef = struct
  type t = string option * func_type
end

(** named export *)
module Export = struct
  type t =
    { name : string
    ; id : int
    }
end

module Global = struct
  module Type = struct
    type nonrec t = mut * val_type
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
    }
end

module Custom = struct
  type t = Uninterpreted of string
end

module Mem = struct
  type t = string option * limits
end

module Module = struct
  module Exports = struct
    type t =
      { global : Export.t list
      ; mem : Export.t list
      ; table : Export.t list
      ; func : Export.t list
      }
  end

  type t =
    { id : string option
    ; types : Typedef.t array
    ; global : (Global.t, Global.Type.t) Runtime.t array
    ; table : (Table.t, Table.Type.t) Runtime.t array
    ; mem : (Mem.t, limits) Runtime.t array
    ; func :
        (Func.t, block_type) Runtime.t array (* TODO: switch to func_type *)
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
    ; exports = { global = []; mem = []; table = []; func = [] }
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
          | Ref_func idx -> Ref_func (update_idx idx)
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
      | Runtime.Imported _ as f -> f
      | Runtime.Local (f : Func.t) ->
        let body = handle_expr f.body in
        Runtime.Local { f with body }
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
          | Runtime.Imported _ as v -> v
          | Local (global : Global.t) ->
            let init = handle_expr global.init in
            Local { global with init } )
        m.global
    in

    let start = Option.map update_idx m.start in

    let exports =
      let func =
        List.map
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

  (** Exports *)

  (** Return the first function exported as [name] if it exists. Return [None]
      otherwise.*)
  let find_exported_func_from_name name m =
    List.find_opt
      (function { Export.name = name'; _ } -> String.equal name name')
      m.exports.func

  (** Imports *)

  (** Return the index of a function imported from a given [modul_name] and
      [func_name] if it exists. Return [None] otherwise. *)
  let find_imported_func_index ~modul_name ~func_name m =
    Array.find_index
      (function
        | Runtime.Imported { Imported.modul; name; assigned_name = _; typ = _ }
          ->
          String.equal modul_name modul && String.equal func_name name
        | Local _ -> false )
      m.func

  (** Finds the index of the last imported function. Will be `~-1` if there are
      no imported functions. *)
  let find_last_import_index m =
    let _i, last =
      Array.fold_left
        (fun (i, last) -> function
          | Runtime.Imported _ -> (succ i, i) | Runtime.Local _ -> (succ i, last) )
        (0, ~-1) m.func
    in
    last

  (** Look for an imported function index, adding it if not already imported. *)
  let add_import_if_not_present ~modul_name ~func_name ~typ m =
    match find_imported_func_index ~modul_name ~func_name m with
    | Some _i -> m
    | None ->
      let f =
        Runtime.Imported
          { Imported.modul = modul_name
          ; name = func_name
          ; assigned_name = None
          ; typ
          }
      in

      let idx = find_last_import_index m + 1 in

      insert_func_at_idx f m idx
end
