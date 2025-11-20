(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

exception Parse_fail of string

val pp_id_opt : Format.formatter -> string option -> unit

val pp_newline : Format.formatter -> unit -> unit

(* identifiers *)

type indice =
  | Text of string
  | Raw of int

val pp_indice : Format.formatter -> indice -> unit

val pp_indice_opt : Format.formatter -> indice option -> unit

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64
  | V128

val num_type_eq : num_type -> num_type -> bool

type nullable =
  | No_null
  | Null

type nonrec mut =
  | Const
  | Var

val is_mut : mut -> bool

type nonrec nn =
  | S32
  | S64

val pp_nn : Format.formatter -> nn -> unit

type nonrec ishape =
  | I8x16
  | I16x8
  | I32x4
  | I64x2

type nonrec fshape =
  | F32x4
  | F64x8

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

type nonrec vibinop =
  | Add
  | Sub

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

(* TODO: inline this *)
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

type nonrec memarg =
  { offset : Int32.t
  ; align : Int32.t
  }

type nonrec limits =
  { min : int
  ; max : int option
  }

val pp_limits : Format.formatter -> limits -> unit

type nonrec mem = string option * limits

val pp_mem : Format.formatter -> mem -> unit

(** Structure *)

(** Types *)

type heap_type =
  | Func_ht
  | Extern_ht

val pp_heap_type : Format.formatter -> heap_type -> unit

type nonrec ref_type = nullable * heap_type

type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

val val_type_eq : val_type -> val_type -> bool

type nonrec param = string option * val_type

type nonrec param_type = param list

type nonrec result_type = val_type list

type nonrec func_type = param_type * result_type

val pp_func_type : Format.formatter -> func_type -> unit

val compare_func_type : func_type -> func_type -> int

val func_type_eq : func_type -> func_type -> bool

type block_type =
  | Bt_ind of indice
  | Bt_raw of (indice option * func_type)

val pp_block_type : Format.formatter -> block_type -> unit

(** Instructions *)

type instr =
  (* Numeric Instructions *)
  | I32_const of Int32.t
  | I64_const of Int64.t
  | F32_const of Float32.t
  | F64_const of Float64.t
  | V128_const of V128.t
  | I_unop of nn * iunop
  | F_unop of nn * funop
  | I_binop of nn * ibinop
  | F_binop of nn * fbinop
  | V_ibinop of ishape * vibinop
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
  | Block of string option * block_type option * expr Annotated.t
  | Loop of string option * block_type option * expr Annotated.t
  | If_else of
      string option * block_type option * expr Annotated.t * expr Annotated.t
  | Br of indice
  | Br_if of indice
  | Br_table of indice array * indice
  | Return
  | Return_call of indice
  | Return_call_indirect of indice * block_type
  | Return_call_ref of block_type
  | Call of indice
  | Call_indirect of indice * block_type
  | Call_ref of indice
  (* extern *)
  | Extern_externalize
  | Extern_internalize

and expr = instr Annotated.t list

module Func : sig
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }

  val pp : Format.formatter -> t -> unit
end

module Typedef : sig
  type t = string option * func_type

  val pp : Format.formatter -> t -> unit
end

module Table : sig
  module Type : sig
    type nonrec t = limits * ref_type

    val pp : Format.formatter -> t -> unit
  end

  type t = string option * Type.t

  val pp : Format.formatter -> t -> unit
end

module Global : sig
  module Type : sig
    type nonrec t = mut * val_type

    val pp : Format.formatter -> t -> unit
  end

  type t =
    { typ : Type.t
    ; init : expr Annotated.t
    ; id : string option
    }

  val pp : Format.formatter -> t -> unit
end

module Data : sig
  module Mode : sig
    type t =
      | Passive
      | Active of indice option * expr Annotated.t
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }

  val pp : Format.formatter -> t -> unit
end

module Elem : sig
  module Mode : sig
    type t =
      | Passive
      | Declarative
      | Active of indice option * expr Annotated.t
  end

  type t =
    { id : string option
    ; typ : ref_type
    ; init : expr Annotated.t list
    ; mode : Mode.t
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
  end

  type t =
    { modul : string
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
  end

  type t =
    { name : string
    ; typ : Type.t
    }
end

module Module : sig
  module Field : sig
    type t =
      | Typedef of Typedef.t
      | Global of Global.t
      | Table of Table.t
      | Mem of mem
      | Func of Func.t
      | Elem of Elem.t
      | Data of Data.t
      | Start of indice
      | Import of Import.t
      | Export of Export.t

    val pp : Format.formatter -> t -> unit
  end

  type t =
    { id : string option
    ; fields : Field.t list
    }

  val pp : Format.formatter -> t -> unit
end
