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

let pp_id_opt fmt = function None -> () | Some i -> pp fmt " %a" pp_id i

let pp_indice (type kind) fmt : kind indice -> unit = function
  | Raw u -> pp_int fmt u
  | Text i -> pp_id fmt i

let pp_indice_opt fmt = function None -> () | Some i -> pp_indice fmt i

let pp_indices fmt ids = pp_list ~pp_sep:pp_space pp_indice fmt ids

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

let pp_packed_type fmt = function I8 -> pp fmt "i8" | I16 -> pp fmt "i16"

type nonrec mut =
  | Const
  | Var

let pp_mut fmt = function Const -> () | Var -> pp fmt "mut"

type nonrec nn =
  | S32
  | S64

let pp_nn fmt = function S32 -> pp fmt "32" | S64 -> pp fmt "64"

type nonrec sx =
  | U
  | S

let pp_sx fmt = function U -> pp fmt "u" | S -> pp fmt "s"

type nonrec iunop =
  | Clz
  | Ctz
  | Popcnt

let pp_iunop fmt = function
  | Clz -> pp fmt "clz"
  | Ctz -> pp fmt "ctz"
  | Popcnt -> pp fmt "popcnt"

type nonrec funop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

let pp_funop fmt = function
  | Abs -> pp fmt "abs"
  | Neg -> pp fmt "neg"
  | Sqrt -> pp fmt "sqrt"
  | Ceil -> pp fmt "ceil"
  | Floor -> pp fmt "floor"
  | Trunc -> pp fmt "trunc"
  | Nearest -> pp fmt "nearest"

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
  | (Add : ibinop) -> pp fmt "add"
  | Sub -> pp fmt "sub"
  | Mul -> pp fmt "mul"
  | Div s -> pp fmt "div_%a" pp_sx s
  | Rem s -> pp fmt "rem_%a" pp_sx s
  | And -> pp fmt "and"
  | Or -> pp fmt "or"
  | Xor -> pp fmt "xor"
  | Shl -> pp fmt "shl"
  | Shr s -> pp fmt "shr_%a" pp_sx s
  | Rotl -> pp fmt "rotl"
  | Rotr -> pp fmt "rotr"

type nonrec fbinop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

let pp_fbinop fmt = function
  | (Add : fbinop) -> pp fmt "add"
  | Sub -> pp fmt "sub"
  | Mul -> pp fmt "mul"
  | Div -> pp fmt "div"
  | Min -> pp fmt "min"
  | Max -> pp fmt "max"
  | Copysign -> pp fmt "copysign"

type nonrec itestop = Eqz

let pp_itestop fmt = function Eqz -> pp fmt "eqz"

type nonrec irelop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

let pp_irelop fmt : irelop -> Unit.t = function
  | Eq -> pp fmt "eq"
  | Ne -> pp fmt "ne"
  | Lt sx -> pp fmt "lt_%a" pp_sx sx
  | Gt sx -> pp fmt "gt_%a" pp_sx sx
  | Le sx -> pp fmt "le_%a" pp_sx sx
  | Ge sx -> pp fmt "ge_%a" pp_sx sx

type nonrec frelop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

let frelop fmt : frelop -> Unit.t = function
  | Eq -> pp fmt "eq"
  | Ne -> pp fmt "ne"
  | Lt -> pp fmt "lt"
  | Gt -> pp fmt "gt"
  | Le -> pp fmt "le"
  | Ge -> pp fmt "ge"

type nonrec memarg =
  { offset : int
  ; align : int
  }

let pp_memarg =
  let pow_2 n =
    assert (n >= 0);
    1 lsl n
  in
  fun fmt { offset; align } ->
    let pp_offset fmt offset = if offset > 0 then pp fmt "offset=%d " offset in
    pp fmt "%aalign=%d" pp_offset offset (pow_2 align)

type nonrec limits =
  { min : int
  ; max : int option
  }

let pp_limits fmt { min; max } =
  match max with None -> pp fmt "%d" min | Some max -> pp fmt "%d %d" min max

type nonrec mem = string option * limits

let pp_mem fmt (id, ty) = pp fmt "(memory%a %a)" pp_id_opt id pp_limits ty

type nonrec final =
  | Final
  | No_final

let pp_final fmt = function
  | Final -> pp fmt "final"
  | No_final -> pp fmt "no_final"

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

let pp_param fmt (id, vt) = pp fmt "(param%a %a)" pp_id_opt id pp_val_type vt

type nonrec 'a param_type = 'a param list

let pp_param_type fmt params = pp_list ~pp_sep:pp_space pp_param fmt params

type nonrec 'a result_type = 'a val_type list

let pp_result_ fmt vt = pp fmt "(result %a)" pp_val_type vt

let pp_result_type fmt results = pp_list ~pp_sep:pp_space pp_result_ fmt results

(* wrap printer to print a space before a non empty list *)
(* TODO or make it an optional arg of pp_list? *)
let with_space_list printer fmt l =
  match l with [] -> () | _l -> pp fmt " %a" printer l

(* TODO: add a third case that only has (pt * rt) and is the only one used in simplified *)
type 'a block_type =
  | Bt_ind : 'a indice -> (< with_ind_bt ; .. > as 'a) block_type
  | Bt_raw :
      ('a indice option * ('a param_type * 'a result_type))
      -> (< .. > as 'a) block_type

let pp_block_type (type kind) fmt : kind block_type -> unit = function
  | Bt_ind ind -> pp fmt "(type %a)" pp_indice ind
  | Bt_raw (_ind, (pt, rt)) ->
    pp fmt "%a%a"
      (with_space_list pp_param_type)
      pt
      (with_space_list pp_result_type)
      rt

let pp_block_type_opt fmt = function
  | None -> ()
  | Some bt -> pp_block_type fmt bt

type nonrec 'a func_type = 'a param_type * 'a result_type

let pp_func_type fmt (params, results) =
  pp fmt "(func%a%a)"
    (with_space_list pp_param_type)
    params
    (with_space_list pp_result_type)
    results

type nonrec 'a table_type = limits * 'a ref_type

let pp_table_type fmt (limits, ref_type) =
  pp fmt "%a %a" pp_limits limits pp_ref_type ref_type

type nonrec 'a global_type = mut * 'a val_type

let pp_global_type fmt (mut, val_type) =
  match mut with
  | Var -> pp fmt "(mut %a)" pp_val_type val_type
  | Const -> pp fmt "%a" pp_val_type val_type

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

let rec pp_instr fmt = function
  | I32_const i -> pp fmt "i32.const %ld" i
  | I64_const i -> pp fmt "i64.const %Ld" i
  | F32_const f -> pp fmt "f32.const %a" Float32.pp f
  | F64_const f -> pp fmt "f64.const %a" Float64.pp f
  | I_unop (n, op) -> pp fmt "i%a.%a" pp_nn n pp_iunop op
  | F_unop (n, op) -> pp fmt "f%a.%a" pp_nn n pp_funop op
  | I_binop (n, op) -> pp fmt "i%a.%a" pp_nn n pp_ibinop op
  | F_binop (n, op) -> pp fmt "f%a.%a" pp_nn n pp_fbinop op
  | I_testop (n, op) -> pp fmt "i%a.%a" pp_nn n pp_itestop op
  | I_relop (n, op) -> pp fmt "i%a.%a" pp_nn n pp_irelop op
  | F_relop (n, op) -> pp fmt "f%a.%a" pp_nn n frelop op
  | I_extend8_s n -> pp fmt "i%a.extend8_s" pp_nn n
  | I_extend16_s n -> pp fmt "i%a.extend16_s" pp_nn n
  | I64_extend32_s -> pp fmt "i64.extend32_s"
  | I32_wrap_i64 -> pp fmt "i32.wrap_i64"
  | I64_extend_i32 sx -> pp fmt "i64.extend_i32_%a" pp_sx sx
  | I_trunc_f (n, n', sx) -> pp fmt "i%a.trunc_f%a_%a" pp_nn n pp_nn n' pp_sx sx
  | I_trunc_sat_f (n, n', sx) ->
    pp fmt "i%a.trunc_sat_f%a_%a" pp_nn n pp_nn n' pp_sx sx
  | F32_demote_f64 -> pp fmt "f32.demote_f64"
  | F64_promote_f32 -> pp fmt "f64.promote_f32"
  | F_convert_i (n, n', sx) ->
    pp fmt "f%a.convert_i%a_%a" pp_nn n pp_nn n' pp_sx sx
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
  | Table_copy (id, id') -> pp fmt "table.copy %a %a" pp_indice id pp_indice id'
  | Table_init (tid, eid) ->
    pp fmt "table.init %a %a" pp_indice tid pp_indice eid
  | Elem_drop id -> pp fmt "elem.drop %a" pp_indice id
  | I_load (n, memarg) -> pp fmt "i%a.load %a" pp_nn n pp_memarg memarg
  | F_load (n, memarg) -> pp fmt "f%a.load %a" pp_nn n pp_memarg memarg
  | I_store (n, memarg) -> pp fmt "i%a.store %a" pp_nn n pp_memarg memarg
  | F_store (n, memarg) -> pp fmt "f%a.store %a" pp_nn n pp_memarg memarg
  | I_load8 (n, sx, memarg) ->
    pp fmt "i%a.load8_%a %a" pp_nn n pp_sx sx pp_memarg memarg
  | I_load16 (n, sx, memarg) ->
    pp fmt "i%a.load16_%a %a" pp_nn n pp_sx sx pp_memarg memarg
  | I64_load32 (sx, memarg) ->
    pp fmt "i64.load32_%a %a" pp_sx sx pp_memarg memarg
  | I_store8 (n, memarg) -> pp fmt "i%a.store8 %a" pp_nn n pp_memarg memarg
  | I_store16 (n, memarg) -> pp fmt "i%a.store16 %a" pp_nn n pp_memarg memarg
  | I64_store32 memarg -> pp fmt "i64.store32 %a" pp_memarg memarg
  | Memory_size -> pp fmt "memory.size"
  | Memory_grow -> pp fmt "memory.grow"
  | Memory_fill -> pp fmt "memory.fill"
  | Memory_copy -> pp fmt "memory.copy"
  | Memory_init id -> pp fmt "memory.init %a" pp_indice id
  | Data_drop id -> pp fmt "data.drop %a" pp_indice id
  | Nop -> pp fmt "nop"
  | Unreachable -> pp fmt "unreachable"
  | Block (id, bt, e) ->
    pp fmt "(block%a%a@\n  @[<v>%a@])" pp_id_opt id pp_block_type_opt bt pp_expr
      e
  | Loop (id, bt, e) ->
    pp fmt "(loop%a%a@\n  @[<v>%a@])" pp_id_opt id pp_block_type_opt bt pp_expr
      e
  | If_else (id, bt, e1, e2) ->
    let pp_else fmt e =
      match e with
      | [] -> ()
      | e -> pp fmt "@\n(else@\n  @[<v>%a@]@\n)" pp_expr e
    in
    pp fmt "(if%a%a@\n  @[<v>(then@\n  @[<v>%a@]@\n)%a@]@\n)" pp_id_opt id
      pp_block_type_opt bt pp_expr e1 pp_else e2
  | Br id -> pp fmt "br %a" pp_indice id
  | Br_if id -> pp fmt "br_if %a" pp_indice id
  | Br_table (ids, id) ->
    pp fmt "br_table %a %a"
      (pp_list ~pp_sep:pp_space pp_indice)
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
    pp fmt "br_on_cast_fail %a %a %a" pp_indice id pp_nullable n pp_heap_type t
  | Ref_eq -> pp fmt "ref.eq"

and pp_expr fmt instrs = pp_list ~pp_sep:pp_newline pp_instr fmt instrs

(* TODO: func and expr should also be parametrised on block type:
   using (param_type, result_type) M.block_type before simplify and directly an indice after *)
type 'a func =
  { type_f : 'a block_type
  ; locals : 'a param list
  ; body : 'a expr
  ; id : string option
  }

let pp_local fmt (id, t) = pp fmt "(local%a %a)" pp_id_opt id pp_val_type t

let pp_locals fmt locals = pp_list ~pp_sep:pp_space pp_local fmt locals

let pp_func : type kind. formatter -> kind func -> unit =
 fun fmt f ->
  (* TODO: typeuse ? *)
  pp fmt "(func%a%a%a@\n  @[<v>%a@]@\n)" pp_id_opt f.id pp_block_type f.type_f
    (with_space_list pp_locals)
    f.locals pp_expr f.body

let pp_funcs fmt (funcs : 'a func list) =
  pp_list ~pp_sep:pp_newline pp_func fmt funcs

(* Tables & Memories *)

type 'a table = string option * 'a table_type

let pp_table fmt (id, ty) = pp fmt "(table%a %a)" pp_id_opt id pp_table_type ty

(* Modules *)

type 'a import_desc =
  | Import_func of string option * 'a block_type
  | Import_table of string option * 'a table_type
  | Import_mem of string option * limits
  | Import_global of string option * 'a global_type

let import_desc fmt : 'a import_desc -> Unit.t = function
  | Import_func (id, t) -> pp fmt "(func%a %a)" pp_id_opt id pp_block_type t
  | Import_table (id, t) -> pp fmt "(table%a %a)" pp_id_opt id pp_table_type t
  | Import_mem (id, t) -> pp fmt "(memory%a %a)" pp_id_opt id pp_limits t
  | Import_global (id, t) ->
    pp fmt "(global%a %a)" pp_id_opt id pp_global_type t

type 'a import =
  { modul : string
  ; name : string
  ; desc : 'a import_desc
  }

let pp_import fmt i =
  pp fmt {|(import "%a" "%a" %a)|} pp_string i.modul pp_string i.name
    import_desc i.desc

type 'a export_desc =
  | Export_func of 'a indice option
  | Export_table of 'a indice option
  | Export_mem of 'a indice option
  | Export_global of 'a indice option

let pp_export_desc fmt = function
  | Export_func id -> pp fmt "(func %a)" pp_indice_opt id
  | Export_table id -> pp fmt "(table %a)" pp_indice_opt id
  | Export_mem id -> pp fmt "(memory %a)" pp_indice_opt id
  | Export_global id -> pp fmt "(global %a)" pp_indice_opt id

type 'a export =
  { name : string
  ; desc : 'a export_desc
  }

let pp_export fmt (e : text export) =
  pp fmt {|(export "%s" %a)|} e.name pp_export_desc e.desc

type 'a storage_type =
  | Val_storage_t of 'a val_type
  | Val_packed_t of packed_type

let pp_storage_type fmt = function
  | Val_storage_t t -> pp_val_type fmt t
  | Val_packed_t t -> pp_packed_type fmt t

type 'a field_type = mut * 'a storage_type

let pp_field_type fmt (m, t) =
  match m with
  | Const -> pp fmt " %a" pp_storage_type t
  | Var -> pp fmt "(%a %a)" pp_mut m pp_storage_type t

type 'a struct_field = string option * 'a field_type list

let pp_fields fmt = pp_list ~pp_sep:pp_space pp_field_type fmt

let pp_struct_field fmt ((n : string option), f) =
  pp fmt "@\n  @[<v>(field%a%a)@]" pp_id_opt n pp_fields f

type 'a struct_type = 'a struct_field list

let pp_struct_type fmt =
  pp fmt "(struct %a)" (pp_list ~pp_sep:pp_space pp_struct_field)

let pp_array_type fmt = pp fmt "(array %a)" pp_field_type

type 'a str_type =
  | Def_struct_t of 'a struct_type
  | Def_array_t of 'a field_type
  | Def_func_t of 'a func_type

let str_type fmt = function
  | Def_struct_t t -> pp_struct_type fmt t
  | Def_array_t t -> pp_array_type fmt t
  | Def_func_t t -> pp_func_type fmt t

type 'a sub_type = final * 'a indice list * 'a str_type

let pp_sub_type fmt (f, ids, t) =
  pp fmt "(sub %a %a %a)" pp_final f pp_indices ids str_type t

type 'a type_def = string option * 'a sub_type

let pp_type_def fmt (id, t) =
  pp fmt "@\n  @[<v>(type%a %a)@]" pp_id_opt id pp_sub_type t

type 'a rec_type = 'a type_def list

let pp_rec_type fmt l =
  match l with
  | [] -> ()
  | [ t ] -> pp_type_def fmt t
  | l -> pp fmt "(rec %a)" (pp_list ~pp_sep:pp_space pp_type_def) l

let pp_start fmt start = pp fmt "(start %a)" pp_indice start

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

let pp_const fmt = function
  | Const_I32 i -> pp fmt "(i32.const %ld)" i
  | Const_I64 i -> pp fmt "(i64.const %Ld)" i
  | Const_F32 f -> pp fmt "(f32.const %a)" Float32.pp f
  | Const_F64 f -> pp fmt "(f64.const %a)" Float64.pp f
  | Const_null rt -> pp fmt "(ref.null %a)" pp_heap_type rt
  | Const_host i -> pp fmt "(ref.host %d)" i
  | Const_extern i -> pp fmt "(ref.extern %d)" i
  | Const_array -> pp fmt "ref.array"
  | Const_eq -> pp fmt "ref.eq"
  | Const_i31 -> pp fmt "ref.i31"
  | Const_struct -> pp fmt "ref.struct"

let pp_consts fmt c = pp_list ~pp_sep:pp_space pp_const fmt c
