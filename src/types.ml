(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Format

exception Trap of string

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

type simplified = < without_string_indices ; without_ind_bt >

(* identifiers *)

type _ indice =
  | Text : string -> < with_string_indices ; .. > indice
  | Raw : int -> < .. > indice

let pp_id fmt id = pp fmt "$%s" id

let pp_id_opt fmt = function None -> () | Some i -> pp_id fmt i

let pp_indice (type kind) fmt : kind indice -> unit = function
  | Raw u -> pp_int fmt u
  | Text i -> pp_id fmt i

let pp_indice_opt fmt = function None -> () | Some i -> pp_indice fmt i

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64

let pp_num_type fmt = function
  | I32 -> pp fmt "i32"
  | I64 -> pp fmt "i64"
  | F32 -> pp fmt "f32"
  | F64 -> pp fmt "f64"

type nullable =
  | No_null
  | Null

let pp_nullable fmt = function
  | No_null ->
    (* TODO: no notation to enforce nonnull ? *)
    pp fmt ""
  | Null -> pp fmt "null"

type nonrec packed_type =
  | I8
  | I16

type nonrec mut =
  | Const
  | Var

type nonrec nn =
  | S32
  | S64

let pp_nn fmt = function S32 -> pp fmt "32" | S64 -> pp fmt "64"

type nonrec sx =
  | U
  | S

type nonrec iunop =
  | Clz
  | Ctz
  | Popcnt

type nonrec funop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

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

type nonrec fbinop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

type nonrec itestop = Eqz

type nonrec irelop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

type nonrec frelop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

type memarg =
  { offset : int
  ; align : int
  }

type nonrec limits =
  { min : int
  ; max : int option
  }

type mem = string option * limits

type final =
  | Final
  | No_final

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
  | Any_ht -> pp fmt "any"
  | None_ht -> pp fmt "none"
  | Eq_ht -> pp fmt "eq"
  | I31_ht -> pp fmt "i31"
  | Struct_ht -> pp fmt "struct"
  | Array_ht -> pp fmt "array"
  | Func_ht -> pp fmt "func"
  | No_func_ht -> pp fmt "nofunc"
  | Extern_ht -> pp fmt "extern"
  | No_extern_ht -> pp fmt "noextern"
  | Def_ht i -> pp fmt "%a" pp_indice i

let pp_heap_type_short fmt = function
  | Any_ht -> pp fmt "anyref"
  | None_ht -> pp fmt "(ref none)"
  | Eq_ht -> pp fmt "eqref"
  | I31_ht -> pp fmt "i31ref"
  | Struct_ht -> pp fmt "(ref struct)"
  | Array_ht -> pp fmt "(ref array)"
  | Func_ht -> pp fmt "funcref"
  | No_func_ht -> pp fmt "nofunc"
  | Extern_ht -> pp fmt "externref"
  | No_extern_ht -> pp fmt "(ref noextern)"
  | Def_ht i -> pp fmt "(ref %a)" pp_indice i

type nonrec 'a ref_type = nullable * 'a heap_type

let pp_ref_type fmt (n, ht) =
  match n with
  | No_null -> pp fmt "%a" pp_heap_type_short ht
  | Null -> pp fmt "(ref null %a)" pp_heap_type ht

type nonrec 'a val_type =
  | Num_type of num_type
  | Ref_type of 'a ref_type

let pp_val_type fmt = function
  | Num_type t -> pp_num_type fmt t
  | Ref_type t -> pp_ref_type fmt t

type nonrec 'a param = string option * 'a val_type

let pp_param fmt (id, vt) = pp fmt "(param %a %a)" pp_id_opt id pp_val_type vt

type nonrec 'a param_type = 'a param list

let pp_param_type fmt params =
  pp_list ~pp_sep:(fun fmt () -> pp fmt " ") pp_param fmt params

type nonrec 'a result_type = 'a val_type list

let pp_result_ fmt vt = pp fmt "(result %a)" pp_val_type vt

let pp_result_type fmt results = pp_list ~pp_sep:pp_space pp_result_ fmt results

(* TODO: add a third case that only has (pt * rt) and is the only one used in simplified *)
type 'a block_type =
  | Bt_ind : 'a indice -> (< with_ind_bt ; .. > as 'a) block_type
  | Bt_raw :
      ('a indice option * ('a param_type * 'a result_type))
      -> (< .. > as 'a) block_type

let pp_block_type (type kind) fmt : kind block_type -> unit = function
  | Bt_ind ind -> pp fmt "(type %a)" pp_indice ind
  | Bt_raw (_ind, (pt, rt)) -> pp fmt "%a %a" pp_param_type pt pp_result_type rt

type nonrec 'a func_type = 'a param_type * 'a result_type

type nonrec 'a table_type = limits * 'a ref_type

type nonrec 'a global_type = mut * 'a val_type

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

(* TODO: func and expr should also be parametrised on block type:
   using (param_type, result_type) M.block_type before simplify and directly an indice after *)
type 'a func =
  { type_f : 'a block_type
  ; locals : 'a param list
  ; body : 'a expr
  ; id : string option
  }

(* Tables & Memories *)

type 'a table = string option * 'a table_type

(* Modules *)

type 'a import_desc =
  | Import_func of string option * 'a block_type
  | Import_table of string option * 'a table_type
  | Import_mem of string option * limits
  | Import_global of string option * 'a global_type

type 'a import =
  { modul : string
  ; name : string
  ; desc : 'a import_desc
  }

type 'a export_desc =
  | Export_func of 'a indice option
  | Export_table of 'a indice option
  | Export_mem of 'a indice option
  | Export_global of 'a indice option

type 'a export =
  { name : string
  ; desc : 'a export_desc
  }

type 'a storage_type =
  | Val_storage_t of 'a val_type
  | Val_packed_t of packed_type

type 'a field_type = mut * 'a storage_type

type 'a struct_field = string option * 'a field_type list

type 'a struct_type = 'a struct_field list

type 'a str_type =
  | Def_struct_t of 'a struct_type
  | Def_array_t of 'a field_type
  | Def_func_t of 'a func_type

type 'a sub_type = final * 'a indice list * 'a str_type

type 'a type_def = string option * 'a sub_type

type 'a rec_type = 'a type_def list

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

module Const = struct
  type nonrec ibinop =
    | Add
    | Sub
    | Mul

  type 'a instr =
    | I32_const of Int32.t
    | I64_const of Int64.t
    | F32_const of Float32.t
    | F64_const of Float64.t
    | Ref_null of 'a heap_type
    | Ref_func of 'a indice
    | Global_get of 'a indice
    | I_binop of nn * ibinop
    | Array_new of 'a indice
    | Array_new_default of 'a indice
    | Ref_i31

  type 'a expr = 'a instr list

  module Pp = struct
    let ibinop fmt : ibinop -> Unit.t = function
      | Add -> pp fmt "add"
      | Sub -> pp fmt "sub"
      | Mul -> pp fmt "mul"

    let instr fmt : 'a instr -> Unit.t = function
      | I32_const i -> pp fmt "i32.const %ld" i
      | I64_const i -> pp fmt "i64.const %Ld" i
      | F32_const f -> pp fmt "f32.const %a" Float32.pp f
      | F64_const f -> pp fmt "f64.const %a" Float64.pp f
      | I_binop (n, op) -> pp fmt "i%a.%a" pp_nn n ibinop op
      | Global_get id -> pp fmt "global.get %a" pp_indice id
      | Ref_null t -> pp fmt "ref.null %a" pp_heap_type t
      | Ref_func fid -> pp fmt "ref.func %a" pp_indice fid
      | Array_new _ -> pp fmt "array.new"
      | Array_new_default _ -> pp fmt "array.new_default"
      | Ref_i31 -> pp fmt "ref.i31"

    let expr fmt instrs =
      pp_list ~pp_sep:(fun fmt () -> pp fmt "@\n") instr fmt instrs
  end
end

module Pp = struct
  let func_type fmt (l, r) =
    pp fmt "(func %a %a)" pp_param_type l pp_result_type r

  let block_type_opt fmt = function
    | None -> ()
    | Some bt -> pp_block_type fmt bt

  let limits fmt { min; max } =
    match max with
    | None -> pp fmt "%d" min
    | Some max -> pp fmt "%d %d" min max

  let table_type fmt (mt, rt) = pp fmt "%a %a" limits mt pp_ref_type rt

  let mut fmt = function Const -> () | Var -> pp fmt "mut"

  let global_type fmt (m, vt) =
    match m with
    | Var -> pp fmt "(mut %a)" pp_val_type vt
    | Const -> pp fmt "%a" pp_val_type vt

  let local fmt (id, t) = pp fmt "(local %a %a)" pp_id_opt id pp_val_type t

  let locals fmt locals = pp_list ~pp_sep:pp_space local fmt locals

  let iunop fmt = function
    | Clz -> pp fmt "clz"
    | Ctz -> pp fmt "ctz"
    | Popcnt -> pp fmt "popcnt"

  let funop fmt = function
    | Abs -> pp fmt "abs"
    | Neg -> pp fmt "neg"
    | Sqrt -> pp fmt "sqrt"
    | Ceil -> pp fmt "ceil"
    | Floor -> pp fmt "floor"
    | Trunc -> pp fmt "trunc"
    | Nearest -> pp fmt "nearest"

  let sx fmt = function U -> pp fmt "u" | S -> pp fmt "s"

  let ibinop fmt = function
    | (Add : ibinop) -> pp fmt "add"
    | Sub -> pp fmt "sub"
    | Mul -> pp fmt "mul"
    | Div s -> pp fmt "div_%a" sx s
    | Rem s -> pp fmt "rem_%a" sx s
    | And -> pp fmt "and"
    | Or -> pp fmt "or"
    | Xor -> pp fmt "xor"
    | Shl -> pp fmt "shl"
    | Shr s -> pp fmt "shr_%a" sx s
    | Rotl -> pp fmt "rotl"
    | Rotr -> pp fmt "rotr"

  let fbinop fmt = function
    | (Add : fbinop) -> pp fmt "add"
    | Sub -> pp fmt "sub"
    | Mul -> pp fmt "mul"
    | Div -> pp fmt "div"
    | Min -> pp fmt "min"
    | Max -> pp fmt "max"
    | Copysign -> pp fmt "copysign"

  let itestop fmt = function Eqz -> pp fmt "eqz"

  let irelop fmt : irelop -> Unit.t = function
    | Eq -> pp fmt "eq"
    | Ne -> pp fmt "ne"
    | Lt s -> pp fmt "lt_%a" sx s
    | Gt s -> pp fmt "gt_%a" sx s
    | Le s -> pp fmt "le_%a" sx s
    | Ge s -> pp fmt "ge_%a" sx s

  let frelop fmt = function
    | (Eq : frelop) -> pp fmt "eq"
    | Ne -> pp fmt "ne"
    | Lt -> pp fmt "lt"
    | Gt -> pp fmt "gt"
    | Le -> pp fmt "le"
    | Ge -> pp fmt "ge"

  let memarg fmt { offset; align } =
    let pow_2 n =
      assert (n >= 0);
      1 lsl n
    in
    let off fmt offset = if offset > 0 then pp fmt "offset=%d" offset in
    pp fmt "%a align=%d" off offset (pow_2 align)

  let rec instr fmt = function
    | I32_const i -> pp fmt "i32.const %ld" i
    | I64_const i -> pp fmt "i64.const %Ld" i
    | F32_const f -> pp fmt "f32.const %a" Float32.pp f
    | F64_const f -> pp fmt "f64.const %a" Float64.pp f
    | I_unop (n, op) -> pp fmt "i%a.%a" pp_nn n iunop op
    | F_unop (n, op) -> pp fmt "f%a.%a" pp_nn n funop op
    | I_binop (n, op) -> pp fmt "i%a.%a" pp_nn n ibinop op
    | F_binop (n, op) -> pp fmt "f%a.%a" pp_nn n fbinop op
    | I_testop (n, op) -> pp fmt "i%a.%a" pp_nn n itestop op
    | I_relop (n, op) -> pp fmt "i%a.%a" pp_nn n irelop op
    | F_relop (n, op) -> pp fmt "f%a.%a" pp_nn n frelop op
    | I_extend8_s n -> pp fmt "i%a.extend8_s" pp_nn n
    | I_extend16_s n -> pp fmt "i%a.extend16_s" pp_nn n
    | I64_extend32_s -> pp fmt "i64.extend32_s"
    | I32_wrap_i64 -> pp fmt "i32.wrap_i64"
    | I64_extend_i32 s -> pp fmt "i64.extend_i32_%a" sx s
    | I_trunc_f (n, n', s) -> pp fmt "i%a.trunc_f%a_%a" pp_nn n pp_nn n' sx s
    | I_trunc_sat_f (n, n', s) ->
      pp fmt "i%a.trunc_sat_f%a_%a" pp_nn n pp_nn n' sx s
    | F32_demote_f64 -> pp fmt "f32.demote_f64"
    | F64_promote_f32 -> pp fmt "f64.promote_f32"
    | F_convert_i (n, n', s) ->
      pp fmt "f%a.convert_i%a_%a" pp_nn n pp_nn n' sx s
    | I_reinterpret_f (n, n') -> pp fmt "i%a.reinterpret_f%a" pp_nn n pp_nn n'
    | F_reinterpret_i (n, n') -> pp fmt "f%a.reinterpret_i%a" pp_nn n pp_nn n'
    | Ref_null t -> pp fmt "ref.null %a" pp_heap_type t
    | Ref_is_null -> pp fmt "ref.is_null"
    | Ref_func fid -> pp fmt "ref.func %a" pp_indice fid
    | Drop -> pp fmt "drop"
    | Select vt -> begin
      match vt with
      | None -> pp fmt "select"
      | Some vt -> pp fmt "select (%a)" pp_result_type vt
      (* TODO: are the parens needed ? *)
    end
    | Local_get id -> pp fmt "local.get %a" pp_indice id
    | Local_set id -> pp fmt "local.set %a" pp_indice id
    | Local_tee id -> pp fmt "local.tee %a" pp_indice id
    | Global_get id -> pp fmt "global.get %a" pp_indice id
    | Global_set id -> pp fmt "global.set %a" pp_indice id
    | Table_get id -> pp fmt "table.get %a" pp_indice id
    | Table_set id -> pp fmt "table.set %a" pp_indice id
    | Table_size id -> pp fmt "table.size %a" pp_indice id
    | Table_grow id -> pp fmt "table.grow %a" pp_indice id
    | Table_fill id -> pp fmt "table.fill %a" pp_indice id
    | Table_copy (id, id') ->
      pp fmt "table.copy %a %a" pp_indice id pp_indice id'
    | Table_init (tid, eid) ->
      pp fmt "table.init %a %a" pp_indice tid pp_indice eid
    | Elem_drop id -> pp fmt "elem.drop %a" pp_indice id
    | I_load (n, ma) -> pp fmt "i%a.load %a" pp_nn n memarg ma
    | F_load (n, ma) -> pp fmt "f%a.load %a" pp_nn n memarg ma
    | I_store (n, ma) -> pp fmt "i%a.store %a" pp_nn n memarg ma
    | F_store (n, ma) -> pp fmt "f%a.store %a" pp_nn n memarg ma
    | I_load8 (n, s, ma) -> pp fmt "i%a.load8_%a %a" pp_nn n sx s memarg ma
    | I_load16 (n, s, ma) -> pp fmt "i%a.load16_%a %a" pp_nn n sx s memarg ma
    | I64_load32 (s, ma) -> pp fmt "i64.load32_%a %a" sx s memarg ma
    | I_store8 (n, ma) -> pp fmt "i%a.store8 %a" pp_nn n memarg ma
    | I_store16 (n, ma) -> pp fmt "i%a.store16 %a" pp_nn n memarg ma
    | I64_store32 ma -> pp fmt "i64.store32 %a" memarg ma
    | Memory_size -> pp fmt "memory.size"
    | Memory_grow -> pp fmt "memory.grow"
    | Memory_fill -> pp fmt "memory.fill"
    | Memory_copy -> pp fmt "memory.copy"
    | Memory_init id -> pp fmt "memory.init %a" pp_indice id
    | Data_drop id -> pp fmt "data.drop %a" pp_indice id
    | Nop -> pp fmt "nop"
    | Unreachable -> pp fmt "unreachable"
    | Block (id, bt, e) ->
      pp fmt "(block %a %a@\n  @[<v>%a@])" pp_id_opt id block_type_opt bt expr e
    | Loop (id, bt, e) ->
      pp fmt "(loop %a %a@\n  @[<v>%a@])" pp_id_opt id block_type_opt bt expr e
    | If_else (id, bt, e1, e2) ->
      pp fmt
        "(if %a %a@\n\
        \  @[<v>(then@\n\
        \  @[<v>%a@]@\n\
         )@\n\
         (else@\n\
        \  @[<v>%a@]@\n\
         )@]@\n\
         )"
        pp_id_opt id block_type_opt bt expr e1 expr e2
    | Br id -> pp fmt "br %a" pp_indice id
    | Br_if id -> pp fmt "br_if %a" pp_indice id
    | Br_table (ids, id) ->
      pp fmt "br_table %a %a"
        (pp_list ~pp_sep:(fun fmt () -> pp fmt " ") pp_indice)
        (Array.to_list ids) pp_indice id
    | Return -> pp fmt "return"
    | Return_call id -> pp fmt "return_call %a" pp_indice id
    | Return_call_indirect (tbl_id, ty_id) ->
      pp fmt "return_call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
    | Return_call_ref ty_id -> pp fmt "return_call_ref %a" pp_block_type ty_id
    | Call id -> pp fmt "call %a" pp_indice id
    | Call_indirect (tbl_id, ty_id) ->
      pp fmt "call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
    | Call_ref ty_id -> pp fmt "call_ref %a" pp_indice ty_id
    | Array_new id -> pp fmt "array.new %a" pp_indice id
    | Array_new_data (id1, id2) ->
      pp fmt "array.new_data %a %a" pp_indice id1 pp_indice id2
    | Array_new_default id -> pp fmt "array.new_default %a" pp_indice id
    | Array_new_elem (id1, id2) ->
      pp fmt "array.new_elem %a %a" pp_indice id1 pp_indice id2
    | Array_new_fixed (id, i) -> pp fmt "array.new_fixed %a %d" pp_indice id i
    | Array_get id -> pp fmt "array.get %a" pp_indice id
    | Array_get_u id -> pp fmt "array.get_u %a" pp_indice id
    | Array_set id -> pp fmt "array.set %a" pp_indice id
    | Array_len -> pp fmt "array.len"
    | Ref_i31 -> pp fmt "ref.i31"
    | I31_get_s -> pp fmt "i31.get_s"
    | I31_get_u -> pp fmt "i31.get_u"
    | Struct_get (i1, i2) -> pp fmt "struct.get %a %a" pp_indice i1 pp_indice i2
    | Struct_get_s (i1, i2) ->
      pp fmt "struct.get_s %a %a" pp_indice i1 pp_indice i2
    | Struct_new i -> pp fmt "struct.new %a" pp_indice i
    | Struct_new_default i -> pp fmt "struct.new_default %a" pp_indice i
    | Struct_set (i1, i2) -> pp fmt "struct.set %a %a" pp_indice i1 pp_indice i2
    | Extern_externalize -> pp fmt "extern.externalize"
    | Extern_internalize -> pp fmt "extern.internalize"
    | Ref_as_non_null -> pp fmt "ref.as_non_null"
    | Ref_cast (n, t) ->
      pp fmt "ref.cast (ref %a %a)" pp_nullable n pp_heap_type t
    | Ref_test (n, t) -> pp fmt "ref.test %a %a" pp_nullable n pp_heap_type t
    | Br_on_non_null id -> pp fmt "br_on_non_null %a" pp_indice id
    | Br_on_null id -> pp fmt "br_on_null %a" pp_indice id
    | Br_on_cast (id, t1, t2) ->
      pp fmt "br_on_cast %a %a %a" pp_indice id pp_ref_type t1 pp_ref_type t2
    | Br_on_cast_fail (id, n, t) ->
      pp fmt "br_on_cast_fail %a %a %a" pp_indice id pp_nullable n pp_heap_type
        t
    | Ref_eq -> pp fmt "ref.eq"

  and expr fmt instrs =
    pp_list ~pp_sep:(fun fmt () -> pp fmt "@\n") instr fmt instrs

  let func : type kind. formatter -> kind func -> unit =
   fun fmt f ->
    (* TODO: typeuse ? *)
    pp fmt "(func %a %a %a@\n  @[<v>%a@]@\n)" pp_id_opt f.id pp_block_type
      f.type_f locals f.locals expr f.body

  let funcs fmt (funcs : 'a func list) =
    pp_list ~pp_sep:(fun fmt () -> pp fmt "@\n") func fmt funcs

  let start fmt start = pp fmt "(start %a)" pp_indice start

  let mem fmt (id, ty) = pp fmt "(memory %a %a)" pp_id_opt id limits ty

  let table fmt (id, ty) = pp fmt "(table %a %a)" pp_id_opt id table_type ty

  let import_desc fmt : 'a import_desc -> Unit.t = function
    | Import_func (id, t) -> pp fmt "(func %a %a)" pp_id_opt id pp_block_type t
    | Import_table (id, t) -> pp fmt "(table %a %a)" pp_id_opt id table_type t
    | Import_mem (id, t) -> pp fmt "(memory %a %a)" pp_id_opt id limits t
    | Import_global (id, t) ->
      pp fmt "(global %a %a)" pp_id_opt id global_type t

  let import fmt i =
    pp fmt {|(import "%a" "%a" %a)|} pp_string i.modul pp_string i.name
      import_desc i.desc

  let packed_type fmt = function I8 -> pp fmt "i8" | I16 -> pp fmt "i16"

  let storage_type fmt = function
    | Val_storage_t t -> pp_val_type fmt t
    | Val_packed_t t -> packed_type fmt t

  let field_type fmt (m, t) =
    match m with
    | Const -> pp fmt " %a" storage_type t
    | Var -> pp fmt "(%a %a)" mut m storage_type t

  let fields fmt = pp_list ~pp_sep:(fun fmt () -> pp fmt " ") field_type fmt

  let struct_field fmt ((n : string option), f) =
    pp fmt "@\n  @[<v>(field %a%a)@]" pp_id_opt n fields f

  let struct_type fmt =
    pp fmt "(struct %a)"
      (pp_list ~pp_sep:(fun fmt () -> pp fmt " ") struct_field)

  let array_type fmt = pp fmt "(array %a)" field_type

  let str_type fmt = function
    | Def_struct_t t -> struct_type fmt t
    | Def_array_t t -> array_type fmt t
    | Def_func_t t -> func_type fmt t

  let indices fmt ids =
    pp_list ~pp_sep:(fun fmt () -> pp fmt " ") pp_indice fmt ids

  let final fmt = function
    | Final -> pp fmt "final"
    | No_final -> pp fmt "no_final"

  let sub_type fmt (f, ids, t) =
    pp fmt "(sub %a %a %a)" final f indices ids str_type t

  let type_def fmt (id, t) =
    pp fmt "@\n  @[<v>(type %a %a)@]" pp_id_opt id sub_type t

  let typ fmt l =
    match l with
    | [] -> ()
    | [ t ] -> type_def fmt t
    | l ->
      pp fmt "(rec %a)" (pp_list ~pp_sep:(fun fmt () -> pp fmt " ") type_def) l
end
