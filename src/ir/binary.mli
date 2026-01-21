(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* identifiers *)

type indice = int

type block_type =
  (* TODO: inline this *)
  | Bt_raw of (indice option * Text.func_type)

(** Instructions *)

type instr =
  (* Numeric Instructions *)
  | I32_const of Int32.t
  | I64_const of Int64.t
  | F32_const of Float32.t
  | F64_const of Float64.t
  | V128_const of Concrete_v128.t
  | I_unop of Text.nn * Text.iunop
  | F_unop of Text.nn * Text.funop
  | I_binop of Text.nn * Text.ibinop
  | F_binop of Text.nn * Text.fbinop
  | V_ibinop of Text.ishape * Text.vibinop
  | I_testop of Text.nn * Text.itestop
  | I_relop of Text.nn * Text.irelop
  | F_relop of Text.nn * Text.frelop
  | I_extend8_s of Text.nn
  | I_extend16_s of Text.nn
  | I64_extend32_s
  | I32_wrap_i64
  | I64_extend_i32 of Text.sx
  | I_trunc_f of Text.nn * Text.nn * Text.sx
  | I_trunc_sat_f of Text.nn * Text.nn * Text.sx
  | F32_demote_f64
  | F64_promote_f32
  | F_convert_i of Text.nn * Text.nn * Text.sx
  | I_reinterpret_f of Text.nn * Text.nn
  | F_reinterpret_i of Text.nn * Text.nn
  (* Reference instructions *)
  | Ref_null of Text.heap_type
  | Ref_is_null
  | Ref_func of indice
  (* Parametric instructions *)
  | Drop
  | Select of Text.val_type list option
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
  | I_load of indice * Text.nn * Text.memarg
  | F_load of indice * Text.nn * Text.memarg
  | I_store of indice * Text.nn * Text.memarg
  | F_store of indice * Text.nn * Text.memarg
  | I_load8 of indice * Text.nn * Text.sx * Text.memarg
  | I_load16 of indice * Text.nn * Text.sx * Text.memarg
  | I64_load32 of indice * Text.sx * Text.memarg
  | I_store8 of indice * Text.nn * Text.memarg
  | I_store16 of indice * Text.nn * Text.memarg
  | I64_store32 of indice * Text.memarg
  | Memory_size of indice
  | Memory_grow of indice
  | Memory_fill of indice
  | Memory_copy of indice * indice
  | Memory_init of indice * indice
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

val pp_instr : short:bool -> instr Fmt.t

val iter_expr : (instr -> unit) -> expr Annotated.t -> unit

module Func : sig
  type t =
    { type_f : block_type
    ; locals : Text.param list
    ; body : expr Annotated.t
    ; id : string option
    }
end

module Export : sig
  (** export *)
  type t =
    { name : string
    ; id : int
    }
end

module Global : sig
  type t =
    { typ : Text.Global.Type.t
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
    ; typ : Text.ref_type (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t list
    ; mode : Mode.t
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
      }
  end

  type t =
    { id : string option
    ; types : Text.Typedef.t array
    ; global : (Global.t, Text.Global.Type.t) Origin.t array
    ; table : (Text.Table.t, Text.Table.Type.t) Origin.t array
    ; mem : (Text.Mem.t, Text.limits) Origin.t array
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
