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

let pp_indice_not0 fmt = function
  | Text i -> pp_id fmt i
  | Raw 0 -> ()
  | Raw u -> Fmt.pf fmt " %d" u

let pp_indice_opt fmt = function None -> () | Some i -> pp_indice fmt i

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

let pp_frelop fmt : frelop -> Unit.t = function
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

(** Instructions *)

type instr =
  (* Numeric Instructions *)
  | I32_const of Int32.t
  | I64_const of Int64.t
  | F32_const of Float32.t
  | F64_const of Float64.t
  | V128_const of Concrete_v128.t
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
  | I_load of indice * nn * memarg
  | F_load of indice * nn * memarg
  | I_store of indice * nn * memarg
  | F_store of indice * nn * memarg
  | I_load8 of indice * nn * sx * memarg
  | I_load16 of indice * nn * sx * memarg
  | I64_load32 of indice * sx * memarg
  | I_store8 of indice * nn * memarg
  | I_store16 of indice * nn * memarg
  | I64_store32 of indice * memarg
  | Memory_size of indice
  | Memory_grow of indice
  | Memory_fill of indice
  | Memory_copy of indice * indice
  | Memory_init of indice * indice
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
  | V128_const f -> pf fmt "v128.const %a" Concrete_v128.pp f
  | I_unop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_iunop op
  | F_unop (n, op) -> pf fmt "f%a.%a" pp_nn n pp_funop op
  | I_binop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_ibinop op
  | F_binop (n, op) -> pf fmt "f%a.%a" pp_nn n pp_fbinop op
  | V_ibinop (shape, op) -> pf fmt "%a.%a" pp_ishape shape pp_vibinop op
  | I_testop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_itestop op
  | I_relop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_irelop op
  | F_relop (n, op) -> pf fmt "f%a.%a" pp_nn n pp_frelop op
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
  | I_load (id, n, memarg) ->
    pf fmt "i%a.load%a %a" pp_nn n pp_indice_not0 id pp_memarg memarg
  | F_load (id, n, memarg) ->
    pf fmt "f%a.load%a %a" pp_nn n pp_indice_not0 id pp_memarg memarg
  | I_store (id, n, memarg) ->
    pf fmt "i%a.store%a %a" pp_nn n pp_indice_not0 id pp_memarg memarg
  | F_store (id, n, memarg) ->
    pf fmt "f%a.store%a %a" pp_nn n pp_indice_not0 id pp_memarg memarg
  | I_load8 (id, n, sx, memarg) ->
    pf fmt "i%a.load8_%a%a %a" pp_nn n pp_sx sx pp_indice_not0 id pp_memarg
      memarg
  | I_load16 (id, n, sx, memarg) ->
    pf fmt "i%a.load16_%a%a %a" pp_nn n pp_sx sx pp_indice_not0 id pp_memarg
      memarg
  | I64_load32 (id, sx, memarg) ->
    pf fmt "i64.load32_%a%a %a" pp_sx sx pp_indice_not0 id pp_memarg memarg
  | I_store8 (id, n, memarg) ->
    pf fmt "i%a.store8%a %a" pp_nn n pp_indice_not0 id pp_memarg memarg
  | I_store16 (id, n, memarg) ->
    pf fmt "i%a.store16%a %a" pp_nn n pp_indice_not0 id pp_memarg memarg
  | I64_store32 (id, memarg) ->
    pf fmt "i64.store32%a %a" pp_indice_not0 id pp_memarg memarg
  | Memory_size id -> pf fmt "memory.size%a" pp_indice_not0 id
  | Memory_grow id -> pf fmt "memory.grow%a" pp_indice_not0 id
  | Memory_fill id -> pf fmt "memory.fill%a" pp_indice_not0 id
  | Memory_copy (Raw 0, Raw 0) -> pf fmt "memory.copy"
  | Memory_copy (id1, id2) ->
    pf fmt "memory.copy %a %a" pp_indice id1 pp_indice id2
  | Memory_init (memidx, dataidx) ->
    pf fmt "memory.init%a %a" pp_indice_not0 memidx pp_indice dataidx
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

module Func = struct
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }

  let pp_local fmt (id, t) = pf fmt "(local%a %a)" pp_id_opt id pp_val_type t

  let pp_locals fmt locals = list ~sep:sp pp_local fmt locals

  let pp fmt f =
    (* TODO: type_use ? *)
    pf fmt "(func%a%a%a@\n  @[<v>%a@]@\n)" pp_id_opt f.id pp_block_type f.type_f
      (with_space_list pp_locals)
      f.locals (pp_expr ~short:false) f.body
end

module Typedef = struct
  type t = string option * func_type

  let pp fmt (id, t) = pf fmt "(type%a %a)" pp_id_opt id pp_func_type t
end

module Table = struct
  module Type = struct
    type nonrec t = limits * ref_type

    let pp fmt (limits, ref_type) =
      pf fmt "%a %a" pp_limits limits pp_ref_type ref_type
  end

  type t = string option * Type.t

  let pp fmt (id, ty) = pf fmt "(table%a %a)" pp_id_opt id Type.pp ty
end

module Global = struct
  module Type = struct
    type nonrec t = mut * val_type

    let pp fmt (mut, val_type) =
      match mut with
      | Var -> pf fmt "(mut %a)" pp_val_type val_type
      | Const -> pf fmt "%a" pp_val_type val_type
  end

  type t =
    { typ : Type.t
    ; init : expr Annotated.t
    ; id : string option
    }

  let pp fmt (g : t) =
    pf fmt "(global%a %a %a)" pp_id_opt g.id Type.pp g.typ
      (pp_expr ~short:false) g.init
end

module Data = struct
  module Mode = struct
    type t =
      | Passive
      | Active of indice option * expr Annotated.t

    let pp fmt = function
      | Passive -> ()
      | Active (i, e) ->
        pf fmt "(memory %a) (offset %a)" pp_indice_opt i (pp_expr ~short:false)
          e
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }

  let pp fmt (d : t) =
    pf fmt {|(data%a %a %S)|} pp_id_opt d.id Mode.pp d.mode d.init
end

module Elem = struct
  module Mode = struct
    type t =
      | Passive
      | Declarative
      | Active of indice option * expr Annotated.t

    let pp fmt = function
      | Passive -> ()
      | Declarative -> pf fmt "declare"
      | Active (i, e) -> (
        match i with
        | None -> pf fmt "(offset %a)" (pp_expr ~short:false) e
        | Some i ->
          pf fmt "(table %a) (offset %a)" pp_indice i (pp_expr ~short:false) e )
  end

  type t =
    { id : string option
    ; typ : ref_type
    ; init : expr Annotated.t list
    ; mode : Mode.t
    }

  let pp_items fmt e = pf fmt "(item %a)" (pp_expr ~short:false) e

  let pp fmt (e : t) =
    pf fmt "@[<hov 2>(elem%a %a %a %a)@]" pp_id_opt e.id Mode.pp e.mode
      pp_ref_type e.typ
      (list ~sep:pp_newline pp_items)
      e.init
end

module Import = struct
  module Type = struct
    type t =
      | Func of string option * block_type
      | Table of string option * Table.Type.t
      | Mem of string option * limits
      | Global of string option * Global.Type.t

    let pp fmt = function
      | Func (id, t) -> pf fmt "(func%a %a)" pp_id_opt id pp_block_type t
      | Table (id, t) -> pf fmt "(table%a %a)" pp_id_opt id Table.Type.pp t
      | Mem (id, t) -> pf fmt "(memory%a %a)" pp_id_opt id pp_limits t
      | Global (id, t) -> pf fmt "(global%a %a)" pp_id_opt id Global.Type.pp t
  end

  type t =
    { modul_name : string
        (** The name of the module from which the import is done *)
    ; name : string  (** The name of the importee in its module of origin *)
    ; typ : Type.t
    }

  let pp fmt i =
    pf fmt {|(import "%a" "%a" %a)|} string i.modul_name string i.name Type.pp
      i.typ
end

module Export = struct
  module Type = struct
    type t =
      | Func of indice option
      | Table of indice option
      | Mem of indice option
      | Global of indice option

    let pp fmt = function
      | Func id -> pf fmt "(func %a)" pp_indice_opt id
      | Table id -> pf fmt "(table %a)" pp_indice_opt id
      | Mem id -> pf fmt "(memory %a)" pp_indice_opt id
      | Global id -> pf fmt "(global %a)" pp_indice_opt id
  end

  type t =
    { name : string
    ; typ : Type.t
    }

  let pp fmt (e : t) = pf fmt {|(export "%s" %a)|} e.name Type.pp e.typ
end

module Mem = struct
  type nonrec t = string option * limits

  let pp fmt (id, ty) = pf fmt "(memory%a %a)" pp_id_opt id pp_limits ty
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
      | Start of indice
      | Import of Import.t
      | Export of Export.t

    let pp_start fmt start = pf fmt "(start %a)" pp_indice start

    let pp fmt = function
      | Typedef t -> Typedef.pp fmt t
      | Global g -> Global.pp fmt g
      | Table t -> Table.pp fmt t
      | Mem m -> Mem.pp fmt m
      | Func f -> Func.pp fmt f
      | Elem e -> Elem.pp fmt e
      | Data d -> Data.pp fmt d
      | Start s -> pp_start fmt s
      | Import i -> Import.pp fmt i
      | Export e -> Export.pp fmt e
  end

  type t =
    { id : string option
    ; fields : Field.t list
    }

  let pp fmt m =
    pf fmt "(module%a@\n  @[<v>%a@]@\n)" pp_id_opt m.id
      (list ~sep:pp_newline Field.pp)
      m.fields
end
