(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
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

type nonrec sx =
  | U
  | S

val pp_sx : sx Fmt.t

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
  | Div of sx
  | Rem of sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx
  | Extend8_s
  | Extend16_s
  | Wrap_i64
  | Trunc_f of nn * sx
  | Trunc_sat_f of nn * sx
  | Reinterpret_f of nn
  | Load of indice * memarg
  | Load8 of indice * sx * memarg
  | Load16 of indice * sx * memarg
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
  | Div of sx
  | Rem of sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx
  | Extend8_s
  | Extend16_s
  | Extend32_s
  | Extend_i32 of sx
  | Trunc_f of nn * sx
  | Trunc_sat_f of nn * sx
  | Reinterpret_f of nn
  | Load of indice * memarg
  | Load8 of indice * sx * memarg
  | Load16 of indice * sx * memarg
  | Load32 of indice * sx * memarg
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
  | Convert_i of nn * sx
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
  | Convert_i of nn * sx
  | Reinterpret_i of nn
  | Load of indice * memarg
  | Store of indice * memarg

(** V128 instructions *)
type v128_instr = Const of Concrete_v128.t

val pp_v128_instr : v128_instr Fmt.t

(** I8x16 instructions *)
type i8x16_instr =
  | Add
  | Sub

val pp_i8x16_instr : i8x16_instr Fmt.t

(** I16x8 instructions *)
type i16x8_instr =
  | Add
  | Sub

val pp_i16x8_instr : i16x8_instr Fmt.t

(* I32x4 instructions *)
type i32x4_instr =
  | Add
  | Sub

val pp_i32x4_instr : i32x4_instr Fmt.t

(** I64x2 instructions *)
type i64x2_instr =
  | Add
  | Sub

val pp_i64x2_instr : i64x2_instr Fmt.t

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
  | Ref of ref_instr
  | Local of local_instr
  | Global of global_instr
  | Table of table_instr
  | Elem of elem_instr
  | Memory of memory_instr
  | Data of data_instr
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
  | Return
  | Return_call of indice
  | Return_call_indirect of indice * block_type
  | Return_call_ref of block_type
  | Call of indice
  | Call_indirect of indice * block_type
  | Call_ref of indice
  (* aggregate types *)
  (* i31 *)
  | Ref_i31
  | I31_get_s
  | I31_get_u
  (* struct *)
  | Struct_new of indice
  | Struct_new_default of indice
  | Struct_get of indice * indice
  | Struct_get_s of indice * indice
  | Struct_get_u of indice * indice
  | Struct_set of indice * indice
  (* array *)
  | Array_new of indice
  | Array_new_default of indice
  | Array_new_fixed of indice * Int32.t
  | Array_new_data of indice * indice
  | Array_new_elem of indice * indice
  | Array_get of indice
  | Array_get_s of indice
  | Array_get_u of indice
  | Array_set of indice
  | Array_len
  | Array_fill of indice
  | Array_copy of indice * indice
  | Array_init_data of indice * indice
  | Array_init_elem of indice * indice
  (* convesion *)
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
