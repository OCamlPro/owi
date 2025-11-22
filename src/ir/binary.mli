(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* identifiers *)

type indice = int

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64
  | V128

type nullable =
  | No_null
  | Null

type nonrec mut =
  | Const
  | Var

type nonrec nn =
  | S32
  | S64

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

type heap_type =
  | Func_ht
  | Extern_ht

type nonrec ref_type = nullable * heap_type

type nonrec val_type =
  | Num_type of num_type
  | Ref_type of ref_type

type nonrec param = string option * val_type

type nonrec param_type = param list

type nonrec result_type = val_type list

type nonrec func_type = param_type * result_type

type block_type =
  (* TODO: inline this *)
  | Bt_raw of (indice option * func_type)

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

val pp_instr : short:bool -> Format.formatter -> instr -> unit

val pp_result_type : Format.formatter -> result_type -> unit

val pp_heap_type : Format.formatter -> heap_type -> unit

val pp_ref_type : Format.formatter -> ref_type -> unit

val pp_num_type : Format.formatter -> num_type -> unit

val num_type_eq : num_type -> num_type -> bool

val val_type_eq : val_type -> val_type -> bool

val ref_type_eq : ref_type -> ref_type -> bool

val heap_type_eq : heap_type -> heap_type -> bool

val func_type_eq : func_type -> func_type -> bool

val compare_func_type : func_type -> func_type -> int

val iter_expr : (instr -> unit) -> expr Annotated.t -> unit

module Func : sig
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }
end

module Table : sig
  module Type : sig
    type nonrec t = limits * ref_type
  end

  type t = string option * Type.t
end

module Typedef : sig
  type t = string option * func_type
end

module Export : sig
  (** export *)
  type t =
    { name : string
    ; id : int
    }
end

module Global : sig
  module Type : sig
    type nonrec t = mut * val_type
  end

  type t =
    { typ : Type.t
    ; init : expr Annotated.t
    ; id : string option
    }
end

module Data : sig
  module Mode : sig
    type t =
      | Passive
      | Active of int * expr Annotated.t
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }
end

module Elem : sig
  module Mode : sig
    type t =
      | Passive
      | Declarative
      (* TODO: Elem_active binary+const expr*)
      | Active of int option * expr Annotated.t
  end

  type t =
    { id : string option
    ; typ : ref_type (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t list
    ; mode : Mode.t
    }
end

module Mem : sig
  type nonrec t = string option * limits
end

module Custom : sig
  type t = Uninterpreted of string
end

module Module : sig
  module Exports : sig
    type t =
      { global : Export.t list
      ; mem : Export.t list
      ; table : Export.t list
      ; func : Export.t list
      }
  end

  type t =
    { id : string option
    ; types : Typedef.t array
    ; global : (Global.t, Global.Type.t) Origin.t array
    ; table : (Table.t, Table.Type.t) Origin.t array
    ; mem : (Mem.t, limits) Origin.t array
    ; func : (Func.t, block_type) Origin.t array (* TODO: switch to func_type *)
    ; elem : Elem.t array
    ; data : Data.t array
    ; exports : Exports.t
    ; start : int option
    ; custom : Custom.t list
    }

  val empty : t

  val add_func : (Func.t, block_type) Origin.t -> t -> t * indice

  val add_import_if_not_present :
    modul_name:string -> func_name:string -> typ:block_type -> t -> t

  val get_func_type : indice -> t -> block_type option

  val find_imported_func_index :
    modul_name:string -> func_name:string -> t -> indice option

  val find_exported_func_from_name : string -> t -> Export.t option
end
