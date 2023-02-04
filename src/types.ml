exception Trap of string

(** Structure *)

(** Types *)

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64

type nullable =
  | No_null
  | Null

type heap_type =
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

type nonrec ref_type = nullable * heap_type

type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

type nonrec packed_type =
  | I8
  | I16

type nonrec param = string option * val_type

type nonrec param_type = param list

type nonrec result_ = val_type

type nonrec result_type = result_ list

type nonrec func_type = param_type * result_type

type nonrec limits =
  { min : int
  ; max : int option
  }

type nonrec mem_type = limits

type nonrec table_type = limits * ref_type

type nonrec mut =
  | Const
  | Var

type nonrec global_type = mut * val_type

type nonrec extern_type =
  | Func of string option * func_type
  | Table of string option * table_type
  | Mem of string option * mem_type
  | Global of string option * global_type

(** Instructions *)

type nonrec nn =
  | S32
  | S64

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

type indice =
  | Raw of int
  | Symbolic of string

type memarg =
  { offset : int
  ; align : int
  }

type 'indice block_type =
  | Bt_ind of 'indice
  | Bt_raw of ('indice option * func_type)
(* the indice option is the optional typeuse, if it's some it must be equal to the func_type *)

type ('indice, 'bt) instr' =
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
  | Ref_null of heap_type
  | Ref_is_null
  | Ref_func of 'indice
  (* Parametric instructions *)
  | Drop
  | Select of val_type list option
  (* Variable instructions *)
  | Local_get of 'indice
  | Local_set of 'indice
  | Local_tee of 'indice
  | Global_get of 'indice
  | Global_set of 'indice
  (* Table instructions *)
  | Table_get of 'indice
  | Table_set of 'indice
  | Table_size of 'indice
  | Table_grow of 'indice
  | Table_fill of 'indice
  | Table_copy of 'indice * 'indice
  | Table_init of 'indice * 'indice
  | Elem_drop of 'indice
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
  | Memory_init of 'indice
  | Data_drop of 'indice
  (* Control instructions *)
  | Nop
  | Unreachable
  | Block of string option * 'bt option * ('indice, 'bt) expr'
  | Loop of string option * 'bt option * ('indice, 'bt) expr'
  | If_else of
      string option * 'bt option * ('indice, 'bt) expr' * ('indice, 'bt) expr'
  | Br of 'indice
  | Br_if of 'indice
  | Br_table of 'indice array * 'indice
  | Return
  | Return_call of 'indice
  | Return_call_indirect of 'indice * 'bt
  | Call of 'indice
  | Call_indirect of 'indice * 'bt

and ('indice, 'bt) expr' = ('indice, 'bt) instr' list

type instr = (indice, indice block_type) instr'

type simplified_instr = (int, int block_type) instr'

type expr = (indice, indice block_type) expr'

type simplified_expr = (int, int block_type) expr'

type result_expr = (int, func_type) expr'

(* TODO: func and expr should also be parametrised on block type:
   using block_type before simplify and directly an indice after *)
type ('indice, 'bt) func' =
  { type_f : 'bt
  ; locals : param list
  ; body : ('indice, 'bt) expr'
  ; id : string option
  }

type 'indice func = ('indice, 'indice block_type) func'

(* Tables & Memories *)

type table = string option * table_type

type mem = string option * mem_type

type 'expr global' =
  { type_ : global_type
  ; init : 'expr
  ; id : string option
  }

type global = (indice, indice block_type) expr' global'

type ('indice, 'expr) elem_mode =
  | Elem_passive
  | Elem_active of 'indice * 'expr
  | Elem_declarative

type ('indice, 'expr) elem' =
  { id : string option
  ; type_ : ref_type
  ; init : 'expr list
  ; mode : ('indice, 'expr) elem_mode
  }

type elem = (indice option, (indice, indice block_type) expr') elem'

type ('indice, 'expr) data_mode =
  | Data_passive
  | Data_active of 'indice * 'expr

type ('indice, 'expr) data' =
  { id : string option
  ; init : string
  ; mode : ('indice, 'expr) data_mode
  }

type data = (indice option, (indice, indice block_type) expr') data'

(* Modules *)

type import_desc =
  | Import_func of string option * indice block_type
  | Import_table of string option * table_type
  | Import_mem of string option * mem_type
  | Import_global of string option * global_type

type import =
  { module_ : string
  ; name : string
  ; desc : import_desc
  }

type 'indice export_desc' =
  | Export_func of 'indice option
  | Export_table of 'indice option
  | Export_mem of 'indice option
  | Export_global of 'indice option

type export_desc = indice export_desc'

type 'indice export' =
  { name : string
  ; desc : 'indice export_desc'
  }

type export = indice export'

type final =
  | Final
  | No_final

type storage_type =
  | Val_storage_t of val_type
  | Val_packed_t of packed_type

type field_type = mut * storage_type

type array_type = field_type

type struct_field = string option * field_type list

type struct_type = struct_field list

type str_type =
  | Def_struct_t of struct_type
  | Def_array_t of array_type
  | Def_func_t of func_type

type sub_type = final * indice list * str_type

type type_def = string option * sub_type

type rec_type = type_def list

type module_field =
  | MType of rec_type
  | MGlobal of global
  | MTable of table
  | MMem of mem
  | MFunc of indice func
  | MElem of elem
  | MData of data
  | MStart of indice
  | MImport of import
  | MExport of export

type module_ =
  { id : string option
  ; fields : module_field list
  }

type const =
  | Const_I32 of Int32.t
  | Const_I64 of Int64.t
  | Const_F32 of Float32.t
  | Const_F64 of Float64.t
  | Const_null of heap_type
  | Const_host of int

type action =
  | Invoke of string option * string * const list
  | Get of string option * string

type result_const =
  | Literal of const
  | Nan_canon of nn
  | Nan_arith of nn

type result =
  | Result_const of result_const
  | Result_extern_ref
  | Result_func_ref

type assert_ =
  | Assert_return of action * result list
  | Assert_trap of action * string
  | Assert_trap_module of module_ * string
  | Assert_malformed of module_ * string
  | Assert_malformed_quote of string list * string
  | Assert_malformed_binary of string list * string
  | Assert_invalid of module_ * string
  | Assert_invalid_quote of string list * string
  | Assert_invalid_binary of string list * string
  | Assert_exhaustion of action * string
  | Assert_unlinkable of module_ * string

type cmd =
  | Module of module_
  | Assert of assert_
  | Register of string * string option
  | Action of action

type script = cmd list

module Const = struct
  type nonrec ibinop =
    | Add
    | Sub
    | Mul

  type instr =
    | I32_const of Int32.t
    | I64_const of Int64.t
    | F32_const of Float32.t
    | F64_const of Float64.t
    | Ref_null of heap_type
    | Ref_func of int
    | Global_get of int
    | I_binop of nn * ibinop

  type expr = instr list
end
