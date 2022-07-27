let page_size = 65_536

exception Trap of string

(** Structure *)

(** Types *)

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64

(* TODO: heap_type missing ? *)
type nonrec ref_type =
  | Func_ref
  | Extern_ref

(* TODO: gadt ? *)
type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

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

(* TODO: the spec is weird, shouldn't it be just `limits` ? is it really a product ? *)
type nonrec table_type = limits * ref_type

type nonrec mut =
  | Const
  | Var

type nonrec global_type = mut * val_type

(* TODO: gadt ? *)
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

type simplified_indice = I of int [@@unboxed]

type memarg =
  { offset : int
  ; align : int
  }

type 'indice block_type =
  | Bt_ind of 'indice
  | Bt_raw of ('indice option * func_type)
(* the indice option is the optional typeuse, if it's some it must be equal to the func_type *)

type 'indice instr =
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
  | Ref_null of ref_type
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
  | Block of string option * 'indice block_type option * 'indice expr
  | Loop of string option * 'indice block_type option * 'indice expr
  | If_else of
      string option * 'indice block_type option * 'indice expr * 'indice expr
  | Br of 'indice
  | Br_if of 'indice
  | Br_table of 'indice array * 'indice
  | Return
  | Call of 'indice
  | Call_indirect of 'indice * 'indice block_type

and 'indice expr = 'indice instr list

(* TODO: func and expr should also be parametrised on block type:
   using block_type before simplify and directly an indice after *)
type ('indice, 'bt) func' =
  { type_f : 'bt
  ; locals : param list
  ; body : 'indice expr
  ; id : string option
  }

type 'indice func = ('indice, 'indice block_type) func'

(* Tables & Memories *)

type table = string option * table_type

type mem = string option * mem_type

type 'indice global' =
  { type_ : global_type
  ; init : 'indice expr
  ; id : string option
  }

type global = indice global'

type 'indice elem_mode =
  | Elem_passive
  | Elem_active of 'indice option * 'indice expr
  | Elem_declarative

type 'indice elem' =
  { id : string option
  ; type_ : ref_type
  ; init : 'indice expr list
  ; mode : 'indice elem_mode
  }

type elem = indice elem'

type 'indice data_mode =
  | Data_passive
  | Data_active of 'indice option * 'indice expr

type 'indice data' =
  { id : string option
  ; init : string
  ; mode : 'indice data_mode
  }

type data = indice data'

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

type type_ = string option * func_type

type module_field =
  | MType of type_
  | MGlobal of indice global'
  | MTable of table
  | MMem of mem
  | MFunc of indice func
  | MElem of indice elem'
  | MData of indice data'
  | MStart of indice
  | MImport of import
  | MExport of indice export'

type module_ =
  { id : string option
  ; fields : module_field list
  }

type const =
  | Const_I32 of Int32.t
  | Const_I64 of Int64.t
  | Const_F32 of Float32.t
  | Const_F64 of Float64.t
  | Const_null of ref_type
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

type file = cmd list
