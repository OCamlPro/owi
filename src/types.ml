let page_size = 65_536

exception Trap of string

(** Structure *)

(** Values *)

(* TODO: this must be utf8 *)
type nonrec name = string

(* TODO: this should only be 0..9 A..Z a..Z ! # $ % & ′ * + - . / : < = > ? @ \ ^ _ ` | ~ *)
type nonrec id = string

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

type nonrec param = id option * val_type

type nonrec param_type = param list

type nonrec result_ = val_type

type nonrec result_type = result_ list

type nonrec func_type = param_type * result_type

type nonrec limits =
  { min : Uint32.t
  ; max : Uint32.t option
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
  | Func of id option * func_type
  | Table of id option * table_type
  | Mem of id option * mem_type
  | Global of id option * global_type

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
  | Raw of Uint32.t
  | Symbolic of id

type memarg =
  { offset : Uint32.t
  ; align : Uint32.t
  }

(* TODO: rename into block type ? *)
type block_type =
  | Bt_ind of indice
  | Bt_raw of func_type

type instr =
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
  | Block of id option * block_type option * expr
  | Loop of id option * block_type option * expr
  | If_else of id option * block_type option * expr * expr
  | Br of indice
  | Br_if of indice
  | Br_table of indice array * indice
  | Return
  | Call of indice
  | Call_indirect of indice * block_type

and expr = instr list

type func =
  { type_f : block_type
  ; locals : param list
  ; body : expr
  ; id : id option
  }

(* Tables & Memories *)

type table = id option * table_type

type mem = id option * mem_type

type global =
  { type_ : global_type
  ; init : expr
  ; id : id option
  }

type elem_mode =
  | Elem_passive
  | Elem_active of indice option * expr
  | Elem_declarative

type elem =
  { type_ : ref_type
  ; init : expr list
  ; mode : elem_mode
  }

type data_mode =
  | Data_passive
  | Data_active of indice option * expr

type data =
  { init : string
  ; mode : data_mode
  }

(* Modules *)

type import_desc =
  | Import_func of id option * block_type
  | Import_table of id option * table_type
  | Import_mem of id option * mem_type
  | Import_global of id option * global_type

type import =
  { module_ : name
  ; name : name
  ; desc : import_desc
  }

type export_desc =
  | Export_func of indice option
  | Export_table of indice option
  | Export_mem of indice option
  | Export_global of indice option

type export =
  { name : name
  ; desc : export_desc
  }

type type_ = id option * func_type

type module_field =
  | MType of type_
  | MGlobal of global
  | MTable of table
  | MMem of mem
  | MFunc of func
  | MElem of elem
  | MData of data
  | MStart of indice
  | MImport of import
  | MExport of export

type module_ =
  { id : id option
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

type num_pat =
  | NP_value of string
  | NP_nan_canon
  | NP_nan_arith

type result =
  | Result_const of const
  | Result_extern_ref
  | Result_func_ref

type assert_ =
  | Assert_return of action * result list
  | Assert_trap of action * string
  | Assert_malformed of module_ * string
  | Assert_malformed_quote of string list * string
  | Assert_malformed_binary of string list * string
  | Assert_invalid of module_ * string
  | Assert_invalid_quote of string list * string
  | Assert_invalid_binary of string list * string

type cmd =
  | Module of module_
  | Assert of assert_
  | Register of string * string option
  | Action of action

type file = cmd list
