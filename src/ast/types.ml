(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

exception Parse_fail of string

type yes = Yes

type no = No

(* IR parameters *)
type with_string_indices = < string_indices : yes >

type without_string_indices = < string_indices : no >

type with_ind_bt = < raw_bt : yes >

type without_ind_bt = < raw_bt : no >

(* various IR *)
type text = < with_string_indices ; with_ind_bt >

type binary = < without_string_indices ; without_ind_bt >

let sp ppf () = Fmt.char ppf ' '

(* identifiers *)

type _ indice =
  | Text : string -> < with_string_indices ; .. > indice
  | Raw : int -> < .. > indice

let pp_id fmt id = pf fmt "$%s" id

let pp_id_opt fmt = function None -> () | Some i -> pf fmt " %a" pp_id i

let pp_indice (type kind) fmt : kind indice -> unit = function
  | Raw u -> int fmt u
  | Text i -> pp_id fmt i

let compare_indice (type a) (id1 : a indice) (id2 : a indice) =
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

let pp_num_type fmt = function
  | I32 -> pf fmt "i32"
  | I64 -> pf fmt "i64"
  | F32 -> pf fmt "f32"
  | F64 -> pf fmt "f64"

let num_type_eq t1 t2 =
  match (t1, t2) with
  | I32, I32 | I64, I64 | F32, F32 | F64, F64 -> true
  | (I32 | I64 | F32 | F64), _ -> false

let compare_num_type t1 t2 =
  let to_int = function I32 -> 0 | I64 -> 1 | F32 -> 2 | F64 -> 3 in
  compare (to_int t1) (to_int t2)

type nullable =
  | No_null
  | Null

let pp_nullable fmt = function
  | No_null ->
    (* TODO: no notation to enforce nonnull ? *)
    pf fmt ""
  | Null -> pf fmt "null"

type nonrec packed_type =
  | I8
  | I16

let pp_packed_type fmt = function I8 -> pf fmt "i8" | I16 -> pf fmt "i16"

let packed_type_eq t1 t2 =
  match (t1, t2) with I8, I8 | I16, I16 -> true | _, _ -> false

type nonrec mut =
  | Const
  | Var

let pp_mut fmt = function Const -> () | Var -> pf fmt "mut"

let is_mut = function Const -> false | Var -> true

type nonrec nn =
  | S32
  | S64

let pp_nn fmt = function S32 -> pf fmt "32" | S64 -> pf fmt "64"

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

let pp_limits fmt { min; max } =
  match max with None -> pf fmt "%d" min | Some max -> pf fmt "%d %d" min max

type nonrec mem = string option * limits

let pp_mem fmt (id, ty) = pf fmt "(memory%a %a)" pp_id_opt id pp_limits ty

type nonrec final =
  | Final
  | No_final

let pp_final fmt = function
  | Final -> pf fmt "final"
  | No_final -> pf fmt "no_final"

let final_eq f1 f2 =
  match (f1, f2) with
  | Final, Final | No_final, No_final -> true
  | _, _ -> false

(** Structure *)

(** Types *)

type 'a heap_type =
  | Any_ht
  | None_ht
  | Eq_ht
  | I31_ht
  | Struct_ht
  | Array_ht
  | Func_ht
  | No_func_ht
  | Extern_ht
  | No_extern_ht
  | Def_ht of 'a indice

let pp_heap_type fmt = function
  | Any_ht -> pf fmt "any"
  | None_ht -> pf fmt "none"
  | Eq_ht -> pf fmt "eq"
  | I31_ht -> pf fmt "i31"
  | Struct_ht -> pf fmt "struct"
  | Array_ht -> pf fmt "array"
  | Func_ht -> pf fmt "func"
  | No_func_ht -> pf fmt "nofunc"
  | Extern_ht -> pf fmt "extern"
  | No_extern_ht -> pf fmt "noextern"
  | Def_ht i -> pf fmt "%a" pp_indice i

let pp_heap_type_short fmt = function
  | Any_ht -> pf fmt "anyref"
  | None_ht -> pf fmt "(ref none)"
  | Eq_ht -> pf fmt "eqref"
  | I31_ht -> pf fmt "i31ref"
  | Struct_ht -> pf fmt "(ref struct)"
  | Array_ht -> pf fmt "(ref array)"
  | Func_ht -> pf fmt "funcref"
  | No_func_ht -> pf fmt "nofunc"
  | Extern_ht -> pf fmt "externref"
  | No_extern_ht -> pf fmt "(ref noextern)"
  | Def_ht i -> pf fmt "(ref %a)" pp_indice i

let heap_type_eq t1 t2 =
  (* TODO: this is wrong *)
  match (t1, t2) with
  | Any_ht, Any_ht
  | None_ht, None_ht
  | Eq_ht, Eq_ht
  | I31_ht, I31_ht
  | Struct_ht, Struct_ht
  | Array_ht, Array_ht
  | Func_ht, Func_ht
  | No_func_ht, No_func_ht
  | Extern_ht, Extern_ht
  | No_extern_ht, No_extern_ht ->
    true
  | Def_ht _, Def_ht _ -> assert false
  | _, _ -> false

let compare_heap_type t1 t2 =
  (* TODO: this is wrong *)
  let to_int = function
    | Any_ht -> 0
    | None_ht -> 1
    | Eq_ht -> 2
    | I31_ht -> 3
    | Struct_ht -> 4
    | Array_ht -> 5
    | Func_ht -> 6
    | No_func_ht -> 7
    | Extern_ht -> 8
    | No_extern_ht -> 9
    | Def_ht _ -> assert false
  in
  Int.compare (to_int t1) (to_int t2)

type nonrec 'a ref_type = nullable * 'a heap_type

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

type nonrec 'a val_type =
  | Num_type of num_type
  | Ref_type of 'a ref_type

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

type nonrec 'a param = string option * 'a val_type

let pp_param fmt (id, vt) = pf fmt "(param%a %a)" pp_id_opt id pp_val_type vt

let param_eq (_, t1) (_, t2) = val_type_eq t1 t2

let compare_param (_, t1) (_, t2) = compare_val_type t1 t2

type nonrec 'a param_type = 'a param list

let pp_param_type fmt params = list ~sep:sp pp_param fmt params

let param_type_eq t1 t2 = List.equal param_eq t1 t2

let compare_param_type t1 t2 = List.compare compare_param t1 t2

type nonrec 'a result_type = 'a val_type list

let pp_result_ fmt vt = pf fmt "(result %a)" pp_val_type vt

let pp_result_type fmt results = list ~sep:sp pp_result_ fmt results

let result_type_eq t1 t2 = List.equal val_type_eq t1 t2

let compare_result_type t1 t2 = List.compare compare_val_type t1 t2

(* wrap printer to print a space before a non empty list *)
(* TODO or make it an optional arg of pp_list? *)
let with_space_list printer fmt l =
  match l with [] -> () | _l -> pf fmt " %a" printer l

type nonrec 'a func_type = 'a param_type * 'a result_type

let pp_func_type fmt (params, results) =
  pf fmt "(func%a%a)"
    (with_space_list pp_param_type)
    params
    (with_space_list pp_result_type)
    results

let func_type_eq (pt1, rt1) (pt2, rt2) =
  param_type_eq pt1 pt2 && result_type_eq rt1 rt2

(* TODO: add a third case that only has (pt * rt) and is the only one used in simplified *)
type 'a block_type =
  | Bt_ind : 'a indice -> (< with_ind_bt ; .. > as 'a) block_type
  | Bt_raw : ('a indice option * 'a func_type) -> (< .. > as 'a) block_type

let pp_block_type (type kind) fmt : kind block_type -> unit = function
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

type nonrec 'a table_type = limits * 'a ref_type

let pp_table_type fmt (limits, ref_type) =
  pf fmt "%a %a" pp_limits limits pp_ref_type ref_type

type nonrec 'a global_type = mut * 'a val_type

let pp_global_type fmt (mut, val_type) =
  match mut with
  | Var -> pf fmt "(mut %a)" pp_val_type val_type
  | Const -> pf fmt "%a" pp_val_type val_type

type nonrec 'a extern_type =
  | Func of string option * 'a func_type
  | Table of string option * 'a table_type
  | Mem of string option * limits
  | Global of string option * 'a global_type

(** Instructions *)

type 'a instr =
  (* { desc : instr_desc
       ; loc : Lexing.position
       }

     and instr_desc =*)
  (* Numeric Instructions *)
  | I32_const of Int32.t
  | I64_const of Int64.t
  | F32_const of Float32.t
  | F64_const of Float64.t
  | I_unop of nn * iunop
  | F_unop of nn * funop
  | I_binop of nn * ibinop
  | F_binop of nn * fbinop
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
  | Ref_null of 'a heap_type
  | Ref_is_null
  | Ref_i31
  | Ref_func of 'a indice
  | Ref_as_non_null
  | Ref_cast of nullable * 'a heap_type
  | Ref_test of nullable * 'a heap_type
  | Ref_eq
  (* Parametric instructions *)
  | Drop
  | Select of 'a val_type list option
  (* Variable instructions *)
  | Local_get of 'a indice
  | Local_set of 'a indice
  | Local_tee of 'a indice
  | Global_get of 'a indice
  | Global_set of 'a indice
  (* Table instructions *)
  | Table_get of 'a indice
  | Table_set of 'a indice
  | Table_size of 'a indice
  | Table_grow of 'a indice
  | Table_fill of 'a indice
  | Table_copy of 'a indice * 'a indice
  | Table_init of 'a indice * 'a indice
  | Elem_drop of 'a indice
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
  | Memory_init of 'a indice
  | Data_drop of 'a indice
  (* Control instructions *)
  | Nop
  | Unreachable
  | Block of string option * 'a block_type option * 'a expr
  | Loop of string option * 'a block_type option * 'a expr
  | If_else of string option * 'a block_type option * 'a expr * 'a expr
  | Br of 'a indice
  | Br_if of 'a indice
  | Br_table of 'a indice array * 'a indice
  | Br_on_cast of 'a indice * 'a ref_type * 'a ref_type
  | Br_on_cast_fail of 'a indice * nullable * 'a heap_type
  | Br_on_non_null of 'a indice
  | Br_on_null of 'a indice
  | Return
  | Return_call of 'a indice
  | Return_call_indirect of 'a indice * 'a block_type
  | Return_call_ref of 'a block_type
  | Call of 'a indice
  | Call_indirect of 'a indice * 'a block_type
  | Call_ref of 'a indice
  (* Array instructions *)
  | Array_get of 'a indice
  | Array_get_u of 'a indice
  | Array_len
  | Array_new of 'a indice
  | Array_new_data of 'a indice * 'a indice
  | Array_new_default of 'a indice
  | Array_new_elem of 'a indice * 'a indice
  | Array_new_fixed of 'a indice * int
  | Array_set of 'a indice
  (* I31 *)
  | I31_get_u
  | I31_get_s
  (* struct*)
  | Struct_get of 'a indice * 'a indice
  | Struct_get_s of 'a indice * 'a indice
  | Struct_new of 'a indice
  | Struct_new_default of 'a indice
  | Struct_set of 'a indice * 'a indice
  (* extern *)
  | Extern_externalize
  | Extern_internalize

and 'a expr = 'a instr list

let pp_newline ppf () = pf ppf "@\n"

let rec pp_instr ~short fmt = function
  | I32_const i -> pf fmt "i32.const %ld" i
  | I64_const i -> pf fmt "i64.const %Ld" i
  | F32_const f -> pf fmt "f32.const %a" Float32.pp f
  | F64_const f -> pf fmt "f64.const %a" Float64.pp f
  | I_unop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_iunop op
  | F_unop (n, op) -> pf fmt "f%a.%a" pp_nn n pp_funop op
  | I_binop (n, op) -> pf fmt "i%a.%a" pp_nn n pp_ibinop op
  | F_binop (n, op) -> pf fmt "f%a.%a" pp_nn n pp_fbinop op
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
      match e with
      | [] -> ()
      | e -> pf fmt "@\n(else@\n  @[<v>%a@]@\n)" (pp_expr ~short) e
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
  | Array_new id -> pf fmt "array.new %a" pp_indice id
  | Array_new_data (id1, id2) ->
    pf fmt "array.new_data %a %a" pp_indice id1 pp_indice id2
  | Array_new_default id -> pf fmt "array.new_default %a" pp_indice id
  | Array_new_elem (id1, id2) ->
    pf fmt "array.new_elem %a %a" pp_indice id1 pp_indice id2
  | Array_new_fixed (id, i) -> pf fmt "array.new_fixed %a %d" pp_indice id i
  | Array_get id -> pf fmt "array.get %a" pp_indice id
  | Array_get_u id -> pf fmt "array.get_u %a" pp_indice id
  | Array_set id -> pf fmt "array.set %a" pp_indice id
  | Array_len -> pf fmt "array.len"
  | Ref_i31 -> pf fmt "ref.i31"
  | I31_get_s -> pf fmt "i31.get_s"
  | I31_get_u -> pf fmt "i31.get_u"
  | Struct_get (i1, i2) -> pf fmt "struct.get %a %a" pp_indice i1 pp_indice i2
  | Struct_get_s (i1, i2) ->
    pf fmt "struct.get_s %a %a" pp_indice i1 pp_indice i2
  | Struct_new i -> pf fmt "struct.new %a" pp_indice i
  | Struct_new_default i -> pf fmt "struct.new_default %a" pp_indice i
  | Struct_set (i1, i2) -> pf fmt "struct.set %a %a" pp_indice i1 pp_indice i2
  | Extern_externalize -> pf fmt "extern.externalize"
  | Extern_internalize -> pf fmt "extern.internalize"
  | Ref_as_non_null -> pf fmt "ref.as_non_null"
  | Ref_cast (n, t) ->
    pf fmt "ref.cast (ref %a %a)" pp_nullable n pp_heap_type t
  | Ref_test (n, t) -> pf fmt "ref.test %a %a" pp_nullable n pp_heap_type t
  | Br_on_non_null id -> pf fmt "br_on_non_null %a" pp_indice id
  | Br_on_null id -> pf fmt "br_on_null %a" pp_indice id
  | Br_on_cast (id, t1, t2) ->
    pf fmt "br_on_cast %a %a %a" pp_indice id pp_ref_type t1 pp_ref_type t2
  | Br_on_cast_fail (id, n, t) ->
    pf fmt "br_on_cast_fail %a %a %a" pp_indice id pp_nullable n pp_heap_type t
  | Ref_eq -> pf fmt "ref.eq"

and pp_expr ~short fmt instrs =
  list ~sep:pp_newline (pp_instr ~short) fmt instrs

let rec iter_expr f (e : _ expr) = List.iter (iter_instr f) e

and iter_instr f (i : _ instr) =
  f i;
  match i with
  | I32_const _ | I64_const _ | F32_const _ | F64_const _
  | I_unop (_, _)
  | F_unop (_, _)
  | I_binop (_, _)
  | F_binop (_, _)
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
  | Ref_null _ | Ref_is_null | Ref_i31 | Ref_func _ | Ref_as_non_null
  | Ref_cast (_, _)
  | Ref_test (_, _)
  | Ref_eq | Drop | Select _ | Local_get _ | Local_set _ | Local_tee _
  | Global_get _ | Global_set _ | Table_get _ | Table_set _ | Table_size _
  | Table_grow _ | Table_fill _
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
  | Br_on_cast (_, _, _)
  | Br_on_cast_fail (_, _, _)
  | Br_on_non_null _ | Br_on_null _ | Return | Return_call _
  | Return_call_indirect (_, _)
  | Return_call_ref _ | Call _
  | Call_indirect (_, _)
  | Call_ref _ | Array_get _ | Array_get_u _ | Array_len | Array_new _
  | Array_new_data (_, _)
  | Array_new_default _
  | Array_new_elem (_, _)
  | Array_new_fixed (_, _)
  | Array_set _ | I31_get_u | I31_get_s
  | Struct_get (_, _)
  | Struct_get_s (_, _)
  | Struct_new _ | Struct_new_default _
  | Struct_set (_, _)
  | Extern_externalize | Extern_internalize ->
    ()
  | Block (_, _, e) | Loop (_, _, e) -> iter_expr f e
  | If_else (_, _, e1, e2) ->
    iter_expr f e1;
    iter_expr f e2

(* TODO: func and expr should also be parametrised on block type:
   using (param_type, result_type) M.block_type before simplify and directly an indice after *)
type 'a func =
  { type_f : 'a block_type
  ; locals : 'a param list
  ; body : 'a expr
  ; id : string option
  }

let pp_local fmt (id, t) = pf fmt "(local%a %a)" pp_id_opt id pp_val_type t

let pp_locals fmt locals = list ~sep:sp pp_local fmt locals

let pp_func : type kind. Format.formatter -> kind func -> unit =
 fun fmt f ->
  (* TODO: typeuse ? *)
  pf fmt "(func%a%a%a@\n  @[<v>%a@]@\n)" pp_id_opt f.id pp_block_type f.type_f
    (with_space_list pp_locals)
    f.locals (pp_expr ~short:false) f.body

let pp_funcs fmt (funcs : 'a func list) = list ~sep:pp_newline pp_func fmt funcs

(* Tables & Memories *)

type 'a table = string option * 'a table_type

let pp_table fmt (id, ty) = pf fmt "(table%a %a)" pp_id_opt id pp_table_type ty

(* Modules *)

type 'a import_desc =
  | Import_func of string option * 'a block_type
  | Import_table of string option * 'a table_type
  | Import_mem of string option * limits
  | Import_global of string option * 'a global_type

let import_desc fmt : 'a import_desc -> Unit.t = function
  | Import_func (id, t) -> pf fmt "(func%a %a)" pp_id_opt id pp_block_type t
  | Import_table (id, t) -> pf fmt "(table%a %a)" pp_id_opt id pp_table_type t
  | Import_mem (id, t) -> pf fmt "(memory%a %a)" pp_id_opt id pp_limits t
  | Import_global (id, t) ->
    pf fmt "(global%a %a)" pp_id_opt id pp_global_type t

type 'a import =
  { modul : string  (** The name of the module from which the import is done *)
  ; name : string  (** The name of the importee in its module of origin *)
  ; desc : 'a import_desc
      (** If this import_desc first field is Some s, the importee is made
          available under name s, else it can only be used via its numerical
          index.*)
  }

let pp_import fmt i =
  pf fmt {|(import "%a" "%a" %a)|} string i.modul string i.name import_desc
    i.desc

type 'a export_desc =
  | Export_func of 'a indice option
  | Export_table of 'a indice option
  | Export_mem of 'a indice option
  | Export_global of 'a indice option

let pp_export_desc fmt = function
  | Export_func id -> pf fmt "(func %a)" pp_indice_opt id
  | Export_table id -> pf fmt "(table %a)" pp_indice_opt id
  | Export_mem id -> pf fmt "(memory %a)" pp_indice_opt id
  | Export_global id -> pf fmt "(global %a)" pp_indice_opt id

type 'a export =
  { name : string
  ; desc : 'a export_desc
  }

let pp_export fmt (e : text export) =
  pf fmt {|(export "%s" %a)|} e.name pp_export_desc e.desc

type 'a storage_type =
  | Val_storage_t of 'a val_type
  | Val_packed_t of packed_type

let pp_storage_type fmt = function
  | Val_storage_t t -> pp_val_type fmt t
  | Val_packed_t t -> pp_packed_type fmt t

let storage_type_eq t1 t2 =
  match (t1, t2) with
  | Val_storage_t t1, Val_storage_t t2 -> val_type_eq t1 t2
  | Val_packed_t t1, Val_packed_t t2 -> packed_type_eq t1 t2
  | _, _ -> false

type 'a field_type = mut * 'a storage_type

let pp_field_type fmt (m, t) =
  match m with
  | Const -> pf fmt " %a" pp_storage_type t
  | Var -> pf fmt "(%a %a)" pp_mut m pp_storage_type t

let field_type_eq t1 t2 =
  match (t1, t2) with
  | (Const, t1), (Const, t2) | (Var, t1), (Var, t2) -> storage_type_eq t1 t2
  | _, _ -> false

type 'a struct_field = string option * 'a field_type list

let pp_fields fmt = list ~sep:sp pp_field_type fmt

let pp_struct_field fmt ((n : string option), f) =
  pf fmt "@\n  @[<v>(field%a%a)@]" pp_id_opt n pp_fields f

let struct_field_eq (_, t1) (_, t2) = List.equal field_type_eq t1 t2

type 'a struct_type = 'a struct_field list

let pp_struct_type fmt = pf fmt "(struct %a)" (list ~sep:sp pp_struct_field)

let struct_type_eq t1 t2 = List.equal struct_field_eq t1 t2

let pp_array_type fmt = pf fmt "(array %a)" pp_field_type

type 'a str_type =
  | Def_struct_t of 'a struct_type
  | Def_array_t of 'a field_type
  | Def_func_t of 'a func_type

let pp_str_type fmt = function
  | Def_struct_t t -> pp_struct_type fmt t
  | Def_array_t t -> pp_array_type fmt t
  | Def_func_t t -> pp_func_type fmt t

let str_type_eq t1 t2 =
  match (t1, t2) with
  | Def_struct_t t1, Def_struct_t t2 -> struct_type_eq t1 t2
  | Def_array_t t1, Def_array_t t2 -> field_type_eq t1 t2
  | Def_func_t t1, Def_func_t t2 -> func_type_eq t1 t2
  | _, _ -> false

let compare_str_type t1 t2 =
  match (t1, t2) with
  | Def_func_t t1, Def_func_t t2 -> compare_func_type t1 t2
  | _, _ -> assert false

type 'a sub_type = final * 'a indice list * 'a str_type

let pp_sub_type fmt (f, ids, t) =
  pf fmt "(sub %a %a %a)" pp_final f pp_indices ids pp_str_type t

let sub_type_eq (f1, ids1, t1) (f2, ids2, t2) =
  final_eq f1 f2 && List.equal indice_eq ids1 ids2 && str_type_eq t1 t2

type 'a type_def = string option * 'a sub_type

let pp_type_def_no_indent fmt (id, t) =
  pf fmt "(type%a %a)" pp_id_opt id pp_sub_type t

let pp_type_def fmt t = pf fmt "@\n  @[<v>%a@]" pp_type_def_no_indent t

let type_def_eq (id1, t1) (id2, t2) =
  Option.equal String.equal id1 id2 && sub_type_eq t1 t2

type 'a rec_type = 'a type_def list

let pp_rec_type fmt l =
  match l with
  | [] -> ()
  | [ t ] -> pf fmt "%a" pp_type_def_no_indent t
  | l -> pf fmt "(rec %a)" (list ~sep:sp pp_type_def) l

let rec_type_eq l1 l2 = List.equal type_def_eq l1 l2

let pp_start fmt start = pf fmt "(start %a)" pp_indice start

type 'a const =
  | Const_I32 of Int32.t
  | Const_I64 of Int64.t
  | Const_F32 of Float32.t
  | Const_F64 of Float64.t
  | Const_null of 'a heap_type
  | Const_host of int
  | Const_extern of int
  | Const_array
  | Const_eq
  | Const_i31
  | Const_struct

let pp_const fmt c =
  pf fmt "(%a)"
    (fun fmt c ->
      match c with
      | Const_I32 i -> pf fmt "i32.const %ld" i
      | Const_I64 i -> pf fmt "i64.const %Ld" i
      | Const_F32 f -> pf fmt "f32.const %a" Float32.pp f
      | Const_F64 f -> pf fmt "f64.const %a" Float64.pp f
      | Const_null rt -> pf fmt "ref.null %a" pp_heap_type rt
      | Const_host i -> pf fmt "ref.host %d" i
      | Const_extern i -> pf fmt "ref.extern %d" i
      | Const_array -> pf fmt "ref.array"
      | Const_eq -> pf fmt "ref.eq"
      | Const_i31 -> pf fmt "ref.i31"
      | Const_struct -> pf fmt "ref.struct" )
    c

let pp_consts fmt c = list ~sep:sp pp_const fmt c
