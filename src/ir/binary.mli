(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* identifiers *)

type indice = int

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

type ref_type = Text.nullable * heap_type

val pp_ref_type : ref_type Fmt.t

val ref_type_eq : ref_type -> ref_type -> bool

type val_type =
  | Num_type of Text.num_type
  | Ref_type of ref_type

type pack_type =
  | I8
  | I16

type storage_type =
  | Val_type of val_type
  | Pack_type of pack_type

val val_type_eq : val_type -> val_type -> bool

val is_subtype_val_type : val_type -> val_type -> bool

type param = string option * val_type

type param_type = param list

type result_type = val_type list

val pp_result_type : Format.formatter -> result_type -> unit

type func_type = param_type * result_type

val pp_func_type : Format.formatter -> func_type -> unit

val func_type_eq : func_type -> func_type -> bool

type block_type =
  (* TODO: inline this *)
  | Bt_raw of (indice option * func_type)

type nonrec memarg =
  { offset : Int64.t
  ; align : Int32.t
  }

val pp_memarg : memarg Fmt.t

(** I32 instructions *)

type i32_instr =
  | Const of Int32.t
  | Clz
  | Ctz
  | Popcnt
  | Add
  | Sub
  | Mul
  | Div of Text.sx
  | Rem of Text.sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of Text.sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of Text.sx
  | Gt of Text.sx
  | Le of Text.sx
  | Ge of Text.sx
  | Extend8_s
  | Extend16_s
  | Wrap_i64
  | Trunc_f of Text.nn * Text.sx
  | Trunc_sat_f of Text.nn * Text.sx
  | Reinterpret_f of Text.nn
  | Load of indice * memarg
  | Load8 of indice * Text.sx * memarg
  | Load16 of indice * Text.sx * memarg
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
  | Div of Text.sx
  | Rem of Text.sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of Text.sx
  | Rotl
  | Rotr
  | Eqz
  | Eq
  | Ne
  | Lt of Text.sx
  | Gt of Text.sx
  | Le of Text.sx
  | Ge of Text.sx
  | Extend8_s
  | Extend16_s
  | Extend32_s
  | Extend_i32 of Text.sx
  | Trunc_f of Text.nn * Text.sx
  | Trunc_sat_f of Text.nn * Text.sx
  | Reinterpret_f of Text.nn
  | Load of indice * memarg
  | Load8 of indice * Text.sx * memarg
  | Load16 of indice * Text.sx * memarg
  | Load32 of indice * Text.sx * memarg
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
  | Convert_i of Text.nn * Text.sx
  | Reinterpret_i of Text.nn
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
  | Convert_i of Text.nn * Text.sx
  | Reinterpret_i of Text.nn
  | Load of indice * memarg
  | Store of indice * memarg

(** Reference instructions *)
type ref_instr =
  | Null of heap_type
  | Is_null
  | As_non_null
  | Func of indice

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

(** Instructions *)
type instr =
  | I32 of i32_instr
  | I64 of i64_instr
  | F32 of f32_instr
  | F64 of f64_instr
  | V128 of Text.v128_instr
  | I8x16 of Text.i8x16_instr
  | I16x8 of Text.i16x8_instr
  | I32x4 of Text.i32x4_instr
  | I64x2 of Text.i64x2_instr
  | Ref of ref_instr
  | Local of local_instr
  | Global of global_instr
  | Table of table_instr
  | Elem of elem_instr
  | Memory of memory_instr
  | Data of data_instr
  | Drop
  | Select of val_type list option
  | Nop
  | Unreachable
  | Block of string option * block_type option * expr Annotated.t
  | Loop of string option * block_type option * expr Annotated.t
  | If_else of
      string option * block_type option * expr Annotated.t * expr Annotated.t
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

and expr = instr Annotated.t list

val pp_instr : short:bool -> instr Fmt.t

val pp_expr : short:bool -> Format.formatter -> expr Annotated.t -> unit

val iter_expr : (instr -> unit) -> expr Annotated.t -> unit

module Func : sig
  type t =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }
end

module Tag : sig
  type t =
    { id : string option
    ; typ : block_type
    }
end

module Export : sig
  (** export *)
  type t =
    { name : string
    ; id : int
    }
end

module Typedef : sig
  type t = string option * func_type

  val pp : t Fmt.t
end

module Table : sig
  module Type : sig
    type limits =
      | I32 of
          { min : Int32.t
          ; max : Int32.t option
          }
      | I64 of
          { min : Int64.t
          ; max : Int64.t option
          }

    val pp_limits : limits Fmt.t

    type nonrec t = limits * ref_type

    val pp : t Fmt.t
  end

  type t =
    { id : string option
    ; typ : Type.t
    ; init : expr Annotated.t option
    }
end

module Mem : sig
  module Type : sig
    type limits =
      | I32 of
          { min : Int32.t
          ; max : Int32.t option
          }
      | I64 of
          { min : int
          ; max : int option
          }

    val pp_limits : limits Fmt.t
  end

  type nonrec t = string option * Type.limits

  val pp : t Fmt.t
end

module Global : sig
  module Type : sig
    type nonrec t = Text.mut * val_type
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
    ; explicit_typ : bool
    }
end

module Custom : sig
  type t = Uninterpreted of string
end

module Module : sig
  module Exports : sig
    type t =
      { global : Export.t Array.t
      ; mem : Export.t Array.t
      ; table : Export.t Array.t
      ; func : Export.t Array.t
      ; tag : Export.t Array.t
      }
  end

  type t =
    { id : string option
    ; types : Typedef.t array
    ; global : (Global.t, Global.Type.t) Origin.t array
    ; table : (Table.t, Table.Type.t) Origin.t array
    ; mem : (Mem.t, Mem.Type.limits) Origin.t array
    ; func : (Func.t, block_type) Origin.t array (* TODO: switch to func_type *)
    ; tag : (Tag.t, block_type) Origin.t array
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

  val get_type : indice -> t -> Typedef.t option

  val find_imported_func_index :
    modul_name:string -> func_name:string -> t -> indice option

  val find_exported_func_from_name : string -> t -> Export.t option
end
