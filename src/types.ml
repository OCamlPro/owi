(** Structure *)

(** Values *)

type nonrec byte = Char.t

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

(* TODO: stop being dumb :-) *)
type nonrec char_ = Uchar.t
type nonrec name = char_ list

(* TODO: this should only be 0..9 A..Z a..Z ! # $ % & ′ * + - . / : < = > ? @ \ ^ _ ` | ~ *)
type id_char = Char.t

type nonrec id = id_char list

(** Types *)

type nonrec num_type =
  | I32 | I64 | F32 | F64

let bit_width = function
  | I32 | F32 -> 32
  | I64 | F64 -> 64

type nonrec ref_type =
  | Func_ref | Extern_ref

(* TODO: gadt ? *)
type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

type nonrec result_type = val_type list

type nonrec func_type = result_type * result_type

type nonrec limits = {
  min : u32;
  max : u32 option;
}

type nonrec mem_type = limits

(* TODO: the spec is weird, shouldn't it be just `limits` ? is it really a product ? *)
type nonrec table_type = limits * ref_type

type nonrec mut =
  | Const | Var

type nonrec global_type = mut * val_type

(* TODO: gadt ? *)
type nonrec extern_type =
  | Func of func_type
  | Table of table_type
  | Mem of mem_type
  | Global of global_type

(** Instructions *)

type nonrec nn = | I32 | I64

type nonrec mm = nn

type nonrec sx = U | S

type nonrec iunop =
  | Clz | Ctz | Popcnt

type nonrec funop =
  | Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest

type nonrec ibinop =
  | Add | Sub | Mul | Div of sx | Rem of sx | And | Or | Xor | Shl | Shr of sx | Rotl | Rotr

type nonrec fbinop =
  | Add | Sub | Mul | Div | Min | Max | Copysign

type nonrec itestop =
  | Eqz

type nonrec irelop =
  | Eq | Ne | Lt of sx | Gt of sx | Le of sx | Ge of sx

type nonrec frelop =
  | Eq | Ne | Lt | Gt | Le | Ge

type nonrec type_idx = u32
type nonrec func_idx = u32
type nonrec table_idx = u32
type nonrec mem_idx = u32
type nonrec global_idx = u32
type nonrec elem_idx = u32
type nonrec data_idx = u32
type nonrec local_idx = u32
type nonrec label_idx = u32

type memarg = {
  offset: u32;
  align: u32;
}

type block_type =
  | Type_idx of type_idx
  | Val_type of val_type option (* TODO: where does the option goes ? *)

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
  | Ref_func of func_idx
  (* Parametric instructions *)
  | Drop
  | Select of (val_type list) option (* TODO: why is it a list and not just tuple ? *)
  (* Variable instructions *)
  | Local_get of local_idx
  | Local_set of local_idx
  | Local_tee of local_idx
  | Global_get of global_idx
  | Global_set of global_idx
  (* Table instructions *)
  | Table_get of table_idx
  | Table_set of table_idx
  | Table_size of table_idx
  | Table_grow of table_idx
  | Table_fill of table_idx
  | Table_copy of table_idx * table_idx
  | Table_init of table_idx * elem_idx
  | Elem_drop of elem_idx
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
  | Memory_init of data_idx
  | Data_drop of data_idx
  (* Control instructions *)
  | Nop
  | Unreachable
  | Block of block_type * (instr list)
  | Loop of block_type * (instr list)
  | If_else of block_type * (instr list) * (instr list)
  | Br of label_idx
  | Br_if of label_idx
  | Br_table of (label_idx list) * label_idx
  | Return
  | Call of func_idx
  | Call_indirect of table_idx * type_idx

type expr = instr list

(** Modules *)

type func = {
  type_ : type_idx;
  locals : val_type list;
  body : expr;
}

type table = table_type

type mem = mem_type

type global = {
  type_ : global_type;
  init : expr;
}

type elem_mode =
  | Passive
  | Active of table_idx * expr (* table * offset *)
  | Declarative

type elem = {
  type_ : ref_type;
  init: expr list;
  mode : elem_mode
}

type data_mode =
  | Passive
  | Active of mem_idx * expr (* memory * offset *)

type data = {
  init : byte list;
  mode : data_mode;
}

type start = func_idx

type import_desc =
  | Func of type_idx
  | Table of table_type
  | Mem of mem_type
  | Global of global_type

type import = {
  module_ : name;
  name : name;
  desc : import_desc;
}

type export_desc =
  | Func of func_idx
  | Table of table_idx
  | Mem of mem_idx
  | Global of global_idx

type export = {
  name : name;
  desc : export_desc;
}

type module_ = {
  id : id option; (* only for text mode *)
  types: func_type list;
  funcs: func list;
  tables: table list;
  mems: mem list;
  globals: global list;
  elems: elem list;
  datas: data list;
  start: start option;
  imports: import list;
  exports: export list;
}
