(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

exception Parse_fail of string

val pp_id_opt : string option Fmt.t

val pp_newline : unit Fmt.t

(* identifiers *)

type indice =
  | Text of string
  | Raw of int

val pp_indice : indice Fmt.t

val pp_indice_opt : indice option Fmt.t

val compare_indice : indice -> indice -> int

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64
  | V128

val pp_num_type : num_type Fmt.t

val num_type_eq : num_type -> num_type -> bool

type nullable =
  | No_null
  | Null

val compare_nullable : nullable -> nullable -> int

type nonrec mut =
  | Const
  | Var

val is_mut : mut -> bool

val pp_mut : mut Fmt.t

type nonrec nn =
  | S32
  | S64

val pp_nn : nn Fmt.t

type nonrec fshape =
  | F32x4
  | F64x8

type nonrec memarg =
  { offset : string option
  ; align : string option
  }

val pp_memarg : memarg Fmt.t

type nonrec limits =
  { is_i64 : bool
  ; min : string
  ; max : string option
  }

val pp_limits : limits Fmt.t

(** Structure *)

(** Types *)

type heap_type =
  | TypeUse of indice
  (* abs_heap_type *)
  | Any_ht
  | Eq_ht
  | I31_ht
  | Struct_ht
  | Array_ht
  | None_ht
  | Func_ht
  | NoFunc_ht
  | Exn_ht
  | NoExn_ht
  | Extern_ht
  | NoExtern_ht

val pp_heap_type : heap_type Fmt.t

val heap_type_eq : heap_type -> heap_type -> bool

type nonrec ref_type = nullable * heap_type

val pp_ref_type : ref_type Fmt.t

val ref_type_eq : ref_type -> ref_type -> bool

val is_subtype_ref_type : ref_type -> ref_type -> bool

type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

val val_type_eq : val_type -> val_type -> bool

val pp_val_type : val_type Fmt.t

val is_subtype_val_type : val_type -> val_type -> bool

type pack_type =
  | I8
  | I16

type storage_type =
  | Val_type of val_type
  | Pack_type of pack_type

val pp_storage_type : storage_type Fmt.t

type nonrec param = string option * val_type

type nonrec param_type = param list

val pp_param_type : param_type Fmt.t

type nonrec result_type = val_type list

val pp_result_type : result_type Fmt.t

type nonrec func_type = param_type * result_type

val pp_func_type : func_type Fmt.t

val compare_func_type : func_type -> func_type -> int

val func_type_eq : func_type -> func_type -> bool

type field_type = mut * storage_type

type field = indice option * field_type

type comp_type =
  | Def_struct_t of field list
  | Def_array_t of field_type
  | Def_func_t of func_type

val pp_comp_type : comp_type Fmt.t

type sub_type =
  { final : bool
  ; ids : indice list
  ; ct : comp_type
  }

val pp_sub_type : sub_type Fmt.t

val sub_type_eq : sub_type -> sub_type -> bool

type block_type =
  | Bt_ind of indice
  | Bt_raw of (indice option * func_type)

val pp_block_type : block_type Fmt.t

(** Instructions *)

(** I32 instructions *)

type i32_instr =
  | Const of Int32.t
  | Clz
  | Ctz
  | Popcnt
  | Add
  | Sub
  | Mul
  | Div_s
  | Div_u
  | Rem_s
  | Rem_u
  | And
  | Or
  | Xor
  | Shl
  | Shr_s
  | Shr_u
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt_s
  | Lt_u
  | Gt_s
  | Gt_u
  | Le_s
  | Le_u
  | Ge_s
  | Ge_u
  | Extend8_s
  | Extend16_s
  | Wrap_i64
  | Trunc_f_s of nn
  | Trunc_f_u of nn
  | Trunc_sat_f_s of nn
  | Trunc_sat_f_u of nn
  | Reinterpret_f of nn
  | Load of indice * memarg
  | Load8_s of indice * memarg
  | Load8_u of indice * memarg
  | Load16_s of indice * memarg
  | Load16_u of indice * memarg
  | Store of indice * memarg
  | Store8 of indice * memarg
  | Store16 of indice * memarg

(** I64 instructions *)

type i64_instr =
  | Const of Int64.t
  | Clz
  | Ctz
  | Popcnt
  | Add
  | Sub
  | Mul
  | Div_s
  | Div_u
  | Rem_s
  | Rem_u
  | And
  | Or
  | Xor
  | Shl
  | Shr_s
  | Shr_u
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt_s
  | Lt_u
  | Gt_s
  | Gt_u
  | Le_s
  | Le_u
  | Ge_s
  | Ge_u
  | Extend8_s
  | Extend16_s
  | Extend32_s
  | Extend_i32_s
  | Extend_i32_u
  | Trunc_f_s of nn
  | Trunc_f_u of nn
  | Trunc_sat_f_s of nn
  | Trunc_sat_f_u of nn
  | Reinterpret_f of nn
  | Load of indice * memarg
  | Load8_s of indice * memarg
  | Load8_u of indice * memarg
  | Load16_s of indice * memarg
  | Load16_u of indice * memarg
  | Load32_s of indice * memarg
  | Load32_u of indice * memarg
  | Store of indice * memarg
  | Store8 of indice * memarg
  | Store16 of indice * memarg
  | Store32 of indice * memarg

(** F32 instructions *)

type f32_instr =
  | Const of Float32.t
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Demote_f64
  | Convert_i_s of nn
  | Convert_i_u of nn
  | Reinterpret_i of nn
  | Load of indice * memarg
  | Store of indice * memarg

(** F64 instructions *)

type f64_instr =
  | Const of Float64.t
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Promote_f32
  | Convert_i_s of nn
  | Convert_i_u of nn
  | Reinterpret_i of nn
  | Load of indice * memarg
  | Store of indice * memarg

(** V128 instructions *)
type v128_instr =
  | Const of Concrete_v128.t
  | Not
  | And
  | Or
  | Any_true
  | Load8_splat of (indice * memarg)
  | Load8_lane of (indice * memarg * int)
  | Load8x8_s of (indice * memarg)
  | Load8x8_u of (indice * memarg)
  | Load16_splat of (indice * memarg)
  | Load16_lane of (indice * memarg * int)
  | Load16x4_s of (indice * memarg)
  | Load16x4_u of (indice * memarg)
  | Load32_splat of (indice * memarg)
  | Load32_lane of (indice * memarg * int)
  | Load32_zero of (indice * memarg)
  | Load64_splat of (indice * memarg)
  | Load64_lane of (indice * memarg * int)
  | Load64_zero of (indice * memarg)
  | Load of (indice * memarg)
  | Store of (indice * memarg)
  | Store8_lane of (indice * memarg * int)
  | Store64_lane of (indice * memarg * int)
  | Store32_zero of (indice * memarg)
  | Store32_lane of (indice * memarg * int)
  | Store16_lane of (indice * memarg * int)
  | Bitselect
  | Xor
  | Load32x2_s of (indice * memarg)
  | Load32x2_u of (indice * memarg)
  | Andnot

val pp_v128_instr : v128_instr Fmt.t

(** I8x16 instructions *)
type i8x16_instr =
  | Add
  | Sub
  | Eq
  | Ne
  | Lt_s
  | Lt_u
  | Gt_s
  | Gt_u
  | Le_s
  | Le_u
  | Ge_s
  | Ge_u
  | Abs
  | Neg
  | Popcnt
  | All_true
  | Bitmask
  | Shuffle of int array (* TODO: make this immutable at some point *)
  | Swizzle
  | Splat
  | Shl
  | Shr_s
  | Shr_u
  | Min_s
  | Min_u
  | Extract_lane_s of int
  | Extract_lane_u of int
  | Add_sat_s
  | Add_sat_u
  | Sub_sat_s
  | Sub_sat_u
  | Max_s
  | Max_u
  | Narrow_i16x8_s
  | Narrow_i16x8_u
  | Avgr_u
  | Replace_lane of int

val pp_i8x16_instr : i8x16_instr Fmt.t

(** I16x8 instructions *)
type i16x8_instr =
  | Add
  | Sub
  | Mul
  | Eq
  | Ne
  | Lt_s
  | Lt_u
  | Gt_s
  | Gt_u
  | Le_s
  | Le_u
  | Ge_s
  | Ge_u
  | Splat
  | Extract_lane_s of int
  | Extract_lane_u of int
  | Q15mulr_sat_s
  | Min_s
  | Min_u
  | Extmul_low_i8x16_s
  | Extmul_low_i8x16_u
  | Extmul_high_i8x16_s
  | Extmul_high_i8x16_u
  | Extend_low_i8x16_s
  | Extend_low_i8x16_u
  | Extend_high_i8x16_s
  | Extend_high_i8x16_u
  | Extadd_pairwise_i8x16_s
  | Extadd_pairwise_i8x16_u
  | Add_sat_s
  | Add_sat_u
  | Sub_sat_s
  | Sub_sat_u
  | Max_s
  | Max_u
  | Shl
  | Neg
  | All_true
  | Shr_s
  | Shr_u
  | Bitmask
  | Avgr_u
  | Abs
  | Replace_lane of int
  | Narrow_i32x4_s
  | Narrow_i32x4_u

val pp_i16x8_instr : i16x8_instr Fmt.t

(* I32x4 instructions *)
type i32x4_instr =
  | Add
  | Sub
  | Mul
  | Shl
  | Shr_s
  | Shr_u
  | Eq
  | Ne
  | Lt_s
  | Lt_u
  | Gt_s
  | Gt_u
  | Le_s
  | Le_u
  | Ge_s
  | Ge_u
  | Splat
  | Extract_lane of int
  | Replace_lane of int
  | Extend_low_i16x8_s
  | Extend_high_i16x8_s
  | Extend_low_i16x8_u
  | Extend_high_i16x8_u
  | Trunc_sat_f64x2_s_zero
  | Trunc_sat_f64x2_u_zero
  | Trunc_sat_f32x4_s
  | Trunc_sat_f32x4_u
  | Min_s
  | Min_u
  | Extmul_low_i16x8_s
  | Extmul_low_i16x8_u
  | Extmul_high_i16x8_s
  | Extmul_high_i16x8_u
  | Extadd_pairwise_i16x8_s
  | Extadd_pairwise_i16x8_u
  | Dot_i16x8_s
  | Neg
  | Max_s
  | Max_u
  | Abs
  | All_true
  | Bitmask

val pp_i32x4_instr : i32x4_instr Fmt.t

(** I64x2 instructions *)
type i64x2_instr =
  | Add
  | Sub
  | Mul
  | Eq
  | Ne
  | Lt_s
  | Gt_s
  | Le_s
  | Ge_s
  | Splat
  | Extend_low_i32x4_s
  | Extend_low_i32x4_u
  | Extend_high_i32x4_s
  | Extend_high_i32x4_u
  | Extmul_low_i32x4_s
  | Extmul_low_i32x4_u
  | Extmul_high_i32x4_s
  | Extmul_high_i32x4_u
  | Abs
  | Neg
  | Extract_lane of int
  | All_true
  | Bitmask
  | Shl
  | Replace_lane of int
  | Shr_s
  | Shr_u

val pp_i64x2_instr : i64x2_instr Fmt.t

type f32x4_instr =
  | Pmin
  | Min
  | Eq
  | Convert_i32x4_s
  | Convert_i32x4_u
  | Ceil
  | Add
  | Max
  | Floor
  | Pmax
  | Ne
  | Sub
  | Abs
  | Trunc
  | Lt
  | Gt
  | Le
  | Ge
  | Mul
  | Convert_low_i32x4_s
  | Convert_low_i32x4_u
  | Convert_high_i32x4_s
  | Convert_high_i32x4_u
  | Splat
  | Nearest
  | Div
  | Neg
  | Extract_lane of int
  | Sqrt
  | Replace_lane of int
  | Demote_f64x2_zero

val pp_f32x4_instr : f32x4_instr Fmt.t

type f64x2_instr =
  | Pmin
  | Min
  | Eq
  | Ceil
  | Add
  | Max
  | Floor
  | Pmax
  | Ne
  | Sub
  | Abs
  | Trunc
  | Lt
  | Gt
  | Le
  | Ge
  | Mul
  | Convert_low_i32x4_s
  | Convert_low_i32x4_u
  | Convert_high_i32x4_s
  | Convert_high_i32x4_u
  | Nearest
  | Div
  | Neg
  | Sqrt
  | Splat
  | Extract_lane of int
  | Promote_low_f32x4
  | Replace_lane of int

val pp_f64x2_instr : f64x2_instr Fmt.t

(** Reference instructions *)
type ref_instr =
  | Null of heap_type
  | Is_null
  | As_non_null
  | Func of indice
  | Eq
  | Test of ref_type
  | Cast of ref_type

(* Local instructions *)
type local_instr =
  | Get of indice
  | Set of indice
  | Tee of indice

(** Global instructions *)
type global_instr =
  | Get of indice
  | Set of indice

(** Table instructions *)
type table_instr =
  | Get of indice
  | Set of indice
  | Size of indice
  | Grow of indice
  | Fill of indice
  | Copy of indice * indice
  | Init of indice * indice

(** Elem instructions *)
type elem_instr = Drop of indice

(** Memory instructions *)
type memory_instr =
  | Size of indice
  | Grow of indice
  | Fill of indice
  | Copy of indice * indice
  | Init of indice * indice

(** Data instructions *)
type data_instr = Drop of indice

(** I31 instructions *)
type i31_instr =
  | Ref
  | Get_s
  | Get_u

val pp_i31_instr : i31_instr Fmt.t

(** Struct instructions *)
type struct_instr =
  | New of indice
  | New_default of indice
  | Get of indice * indice
  | Get_s of indice * indice
  | Get_u of indice * indice
  | Set of indice * indice

(** Array instructions *)
type array_instr =
  | New of indice
  | New_default of indice
  | New_fixed of indice * Int32.t
  | New_data of indice * indice
  | New_elem of indice * indice
  | Get of indice
  | Get_s of indice
  | Get_u of indice
  | Set of indice
  | Len
  | Fill of indice
  | Copy of indice * indice
  | Init_data of indice * indice
  | Init_elem of indice * indice

type instr =
  | I32 of i32_instr
  | I64 of i64_instr
  | F32 of f32_instr
  | F64 of f64_instr
  | V128 of v128_instr
  | I8x16 of i8x16_instr
  | I16x8 of i16x8_instr
  | I32x4 of i32x4_instr
  | I64x2 of i64x2_instr
  | F32x4 of f32x4_instr
  | F64x2 of f64x2_instr
  | Ref of ref_instr
  | Local of local_instr
  | Global of global_instr
  | Table of table_instr
  | Elem of elem_instr
  | Memory of memory_instr
  | Data of data_instr
  | I31 of i31_instr
  | Struct of struct_instr
  | Array of array_instr
  (* Parametric instructions *)
  | Drop
  | Select of val_type list option
  | Nop
  | Unreachable
  | Block of string option * block_type option * expr
  | Loop of string option * block_type option * expr
  | If_else of string option * block_type option * expr * expr
  | Br of indice
  | Br_if of indice
  | Br_table of indice array * indice
  | Br_on_null of indice
  | Br_on_non_null of indice
  | Br_on_cast of indice * ref_type * ref_type
  | Br_on_cast_fail of indice * ref_type * ref_type
  | Return
  | Return_call of indice
  | Return_call_indirect of indice * block_type
  | Return_call_ref of block_type
  | Call of indice
  | Call_indirect of indice * block_type
  | Call_ref of indice
  (* GC convesion instructions *)
  | Any_convert_extern
  | Extern_convert_any

and expr = instr list

val pp_instr : short:bool -> instr Fmt.t

val pp_expr : short:bool -> expr Fmt.t

module Func : sig
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr
    ; id : string option
    }

  val pp : t Fmt.t
end

module Typedef : sig
  type t =
    | SimpleType of (string option * sub_type)
    | RecType of (string option * sub_type) list

  val pp : t Fmt.t
end

module Table : sig
  module Type : sig
    type nonrec t = limits * ref_type

    val pp : t Fmt.t
  end

  type t =
    { id : string option
    ; typ : Type.t
    ; init : expr option
    }

  val pp : t Fmt.t
end

module Global : sig
  module Type : sig
    type nonrec t = mut * val_type

    val pp : t Fmt.t
  end

  type t =
    { typ : Type.t
    ; init : expr
    ; id : string option
    }

  val pp : t Fmt.t
end

module Data : sig
  module Mode : sig
    type t =
      | Passive
      | Active of indice option * expr
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }

  val pp : t Fmt.t
end

module Elem : sig
  module Mode : sig
    type t =
      | Passive
      | Declarative
      | Active of indice option * expr
  end

  type t =
    { id : string option
    ; typ : ref_type
    ; init : expr list
    ; mode : Mode.t
    ; explicit_typ : bool
    }

  val pp : t Fmt.t
end

module Tag : sig
  type t =
    { id : string option
    ; typ : block_type
    }

  val pp : Format.formatter -> t -> unit
end

module Import : sig
  module Type : sig
    type t =
      | Func of string option * block_type
      | Table of string option * Table.Type.t
      | Mem of string option * limits
      | Global of string option * Global.Type.t
      | Tag of string option * block_type
  end

  type t =
    { modul_name : string
        (** The name of the module from which the import is done *)
    ; name : string  (** The name of the importee in its module of origin *)
    ; typ : Type.t
    }
end

module Export : sig
  module Type : sig
    type t =
      | Func of indice option
      | Table of indice option
      | Mem of indice option
      | Global of indice option
      | Tag of indice option
  end

  type t =
    { name : string
    ; typ : Type.t
    }
end

module Mem : sig
  type nonrec t = string option * limits

  val pp : t Fmt.t
end

module Module : sig
  module Field : sig
    type t =
      | Typedef of Typedef.t
      | Global of Global.t
      | Table of Table.t
      | Mem of Mem.t
      | Func of Func.t
      | Elem of Elem.t
      | Data of Data.t
      | Tag of Tag.t
      | Start of indice
      | Import of Import.t
      | Export of Export.t

    val pp : t Fmt.t
  end

  type t =
    { id : string option
    ; fields : Field.t list
    }

  val pp : t Fmt.t

  val pp_fields : Format.formatter -> Field.t list -> unit
end
