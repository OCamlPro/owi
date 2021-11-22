let pp_pos out { Ppxlib.pos_lnum; pos_cnum; pos_bol; _ } =
  Format.fprintf out "line %d:%d" pos_lnum (pos_cnum - pos_bol)

(** Structure *)

(** Values *)

type nonrec u8 = Unsigned.UInt8.t

type nonrec u16 = Unsigned.UInt16.t

type nonrec u32 = Unsigned.UInt32.t

type nonrec u64 = Unsigned.UInt64.t

(* TODO: no Int8 module ? *)
type nonrec s8 = Signed.Int32.t

(* TODO: no Int16 module ? *)
type nonrec s16 = Signed.Int32.t

type nonrec s32 = Signed.Int32.t

type nonrec s64 = Signed.Int64.t

type nonrec i8 = s8

type nonrec i16 = s16

type nonrec i32 = s32

type nonrec i64 = s64

(* TODO: Float32 module ? *)
type nonrec f32 = Float.t

type nonrec f64 = Float.t

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

let bit_width = function
  | I32
  | F32 ->
    32
  | I64
  | F64 ->
    64

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

type nonrec func_type = result_type * result_type

type nonrec limits =
  { min : u32
  ; max : u32 option
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

type nonrec mm = nn

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
  | Raw of u32
  | Symbolic of id

type memarg =
  { offset : u32
  ; align : u32
  }

type block_type =
  | Type_idx of indice
  | Val_type of val_type option
(* TODO: where does the option goes ? *)

type instr =
  (* Numeric Instructions *)
  | I32_const of i32
  | I64_const of i64
  | F32_const of f32
  | F64_const of f64
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
  | Select of val_type list option (* TODO: why is it a list and not just tuple ? *)
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
  | Block of block_type * expr
  | Loop of block_type * expr
  | If_else of block_type * expr * expr
  | Br of indice
  | Br_if of indice
  | Br_table of indice list * indice
  | Return
  | Call of indice
  | Call_indirect of indice * indice

and expr = instr list

type func_type_bis =
  | FTId of indice
  | FTFt of func_type

type func =
  { type_f : func_type_bis
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
  | Elem_active of indice * expr
  | Elem_declarative

type elem =
  { type_ : ref_type
  ; init : expr list
  ; mode : elem_mode
  }

type data_mode =
  | Data_passive
  | Data_active of indice * expr

type data =
  { init : string
  ; mode : data_mode
  }

(* Modules *)

type start = indice

type import_desc =
  | Import_func of id option * func_type_bis
  | Import_table of id option * table_type
  | Import_mem of id option * mem_type
  | Import_global of id option * global_type

type import =
  { module_ : name
  ; name : name
  ; desc : import_desc
  }

type export_desc =
  | Export_func of indice
  | Export_table of indice
  | Export_mem of indice
  | Export_global of indice

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
  | MStart of start
  | MImport of import
  | MExport of export

type module_ =
  { id : id option
  ; fields : module_field list
  }

type const =
  | Const_I32 of i32
  | Const_I64 of i64
  | Const_F32 of f32
  | Const_F64 of f64
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

type failure = string

type assert_ =
  | Assert_return of action * result list
  | Assert_trap of action * failure
  | Assert_malformed of module_ * failure
  | Assert_malformed_quote of string list * failure
  | Assert_malformed_binary of string list * failure
  | Assert_invalid of module_ * failure
  | Assert_invalid_quote of string list * failure
  | Assert_invalid_binary of string list * failure

type cmd =
  | Module of module_
  | Assert of assert_
  | Register of string * string option
  | Action of action

type file = cmd list
