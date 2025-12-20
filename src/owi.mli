module Result : sig
  type err =
    [ `Alignment_too_large
    | `Assert_failure
    | `Bad_result
    | `Call_stack_exhausted
    | `Constant_expression_required
    | `Constant_out_of_range
    | `Did_not_fail_but_expected of string
    | `Duplicate_export_name
    | `Duplicate_global of string
    | `Duplicate_local of string
    | `Duplicate_memory of string
    | `Duplicate_table of string
    | `Failed_with_but_expected of err * string
    | `Found_bug of int
    | `Global_is_immutable
    | `Illegal_escape of string
    | `Import_after_function
    | `Import_after_global
    | `Import_after_memory
    | `Import_after_table
    | `Incompatible_import_type of string
    | `Inline_function_type
    | `Invalid_character_in_memory
    | `Invalid_result_arity
    | `Lexer_illegal_character of string
    | `Lexer_unknown_operator of string
    | `Malformed_utf8_encoding of string
    | `Memory_size_too_large
    | `Msg of string
    | `Multiple_start_sections
    | `No_error
    | `Parse_fail of string
    | `Size_minimum_greater_than_maximum
    | `Start_function
    | `Timeout
    | `Type_mismatch of string
    | `Unbound_last_module
    | `Unbound_module of string
    | `Unbound_name of string
    | `Undeclared_function_reference
    | `Unexpected_token of string
    | `Unknown_data of Text.indice
    | `Unknown_elem of Text.indice
    | `Unknown_func of Text.indice
    | `Unknown_global of Text.indice
    | `Unknown_import of string * string
    | `Unknown_label of Text.indice
    | `Unknown_local of Text.indice
    | `Unknown_memory of Text.indice
    | `Unknown_export of Text.indice
    | `Unknown_module of string
    | `Unknown_operator
    | `Unknown_table of Text.indice
    | `Unknown_type of Text.indice
    | `Unsupported_file_extension of string
    | `Contract_unknown_func of Text.indice
    | `Empty_annotation_id
    | `Empty_identifier
    | `Unclosed_annotation
    | `Unclosed_comment
    | `Unclosed_string
    | `Unbounded_quantification
    | `Invalid_model of string
    | `Unimplemented of string
    | (* Trap: *)
      `Out_of_bounds_table_access
    | `Out_of_bounds_memory_access
    | `Undefined_element
    | `Uninitialized_element of int
    | `Integer_overflow
    | `Integer_divide_by_zero
    | `Conversion_to_integer
    | `Element_type_error
    | `Unreachable
    | `Indirect_call_type_mismatch
    | `Extern_call_arg_type_mismatch
    | `Extern_call_null_arg
    | `Memory_leak_use_after_free
    | `Memory_heap_buffer_overflow
    | `Double_free
    ]

  type 'a t = ('a, err) Prelude.Result.t

  val err_to_string : err -> string

  val err_to_exit_code : err -> int
end

module Annotated : sig
  type 'a t

  val dummy : 'a -> 'a t

  val dummies : 'a list -> 'a t list

  val dummy_deep : 'a list -> 'a t list t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val iter : ('a -> Unit.t) -> 'a t -> Unit.t

  val raw : 'a t -> 'a
end

module Concrete_boolean : sig
  type t

  val to_bool : t -> bool

  val pp : t Fmt.t
end

module Concrete_i32 : sig
  type t

  val zero : t

  val of_int : Int.t -> t

  val of_int32 : Int32.t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val lt : t -> t -> Concrete_boolean.t

  val gt : t -> t -> Concrete_boolean.t

  val le : t -> t -> Concrete_boolean.t

  val ge : t -> t -> Concrete_boolean.t

  val pp : t Fmt.t
end

module Concrete_i64 : sig
  type t

  val of_int64 : Int64.t -> t

  val to_int64 : t -> Int64.t

  val add : t -> t -> t

  val lt : t -> t -> Concrete_boolean.t

  val pp : t Fmt.t
end

module Concrete_f32 : sig
  type t

  val of_float : float -> t

  val pp : t Fmt.t
end

module Concrete_f64 : sig
  type t

  val of_float : float -> t

  val pp : t Fmt.t
end

module Concrete_v128 : sig
  type t

  val of_i64x2 : int64 -> int64 -> t

  val pp : t Fmt.t
end

module Text : sig
  type indice =
    | Text of string
    | Raw of int

  val pp_indice : indice Fmt.t

  val pp_indice_opt : indice option Fmt.t

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

  val pp_nn : nn Fmt.t

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
    { offset : Concrete_i32.t
    ; align : Concrete_i32.t
    }

  type nonrec limits =
    { min : int
    ; max : int option
    }

  val pp_limits : limits Fmt.t

  (** Structure *)

  (** Types *)

  type heap_type =
    | Func_ht
    | Extern_ht

  val pp_heap_type : heap_type Fmt.t

  val heap_type_eq : heap_type -> heap_type -> bool

  type nonrec ref_type = nullable * heap_type

  type nonrec val_type =
    | Num_type of num_type
    | Ref_type of ref_type

  val val_type_eq : val_type -> val_type -> bool

  type nonrec param = string option * val_type

  type nonrec param_type = param list

  type nonrec result_type = val_type list

  type nonrec func_type = param_type * result_type

  val pp_func_type : func_type Fmt.t

  val compare_func_type : func_type -> func_type -> int

  val func_type_eq : func_type -> func_type -> bool

  type block_type =
    | Bt_ind of indice
    | Bt_raw of (indice option * func_type)

  val pp_block_type : block_type Fmt.t

  (** Instructions *)

  type instr =
    (* Numeric Instructions *)
    | I32_const of Concrete_i32.t
    | I64_const of Concrete_i64.t
    | F32_const of Concrete_f32.t
    | F64_const of Concrete_f64.t
    | V128_const of Concrete_v128.t
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
    | I_load of indice * nn * memarg
    | F_load of indice * nn * memarg
    | I_store of indice * nn * memarg
    | F_store of indice * nn * memarg
    | I_load8 of indice * nn * sx * memarg
    | I_load16 of indice * nn * sx * memarg
    | I64_load32 of indice * sx * memarg
    | I_store8 of indice * nn * memarg
    | I_store16 of indice * nn * memarg
    | I64_store32 of indice * memarg
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

  module Func : sig
    type t =
      { type_f : block_type
      ; locals : param list
      ; body : expr Annotated.t
      ; id : string option
      }

    val pp : t Fmt.t
  end

  module Typedef : sig
    type t = string option * func_type

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
      ; init : expr Annotated.t option
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
      ; init : expr Annotated.t
      ; id : string option
      }

    val pp : t Fmt.t
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

    val pp : t Fmt.t
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
      ; explicit_typ : bool
      }

    val pp : t Fmt.t
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
  end
end

module Binary : sig
  type indice = int

  type block_type =
    (* TODO: inline this *)
    | Bt_raw of (indice option * Text.func_type)

  (** Instructions *)

  type instr =
    (* Numeric Instructions *)
    | I32_const of Concrete_i32.t
    | I64_const of Concrete_i64.t
    | F32_const of Concrete_f32.t
    | F64_const of Concrete_f64.t
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

  module Table : sig
    type t =
      { id : string option
      ; typ : Text.Table.Type.t
      ; init : expr Annotated.t option
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
        }
    end

    type t =
      { id : string option
      ; types : Text.Typedef.t array
      ; global : (Global.t, Text.Global.Type.t) Origin.t array
      ; table : (Table.t, Text.Table.Type.t) Origin.t array
      ; mem : (Text.Mem.t, Text.limits) Origin.t array
      ; func :
          (Func.t, block_type) Origin.t array (* TODO: switch to func_type *)
      ; elem : Elem.t array
      ; data : Data.t array
      ; exports : Exports.t
      ; start : int option
      ; custom : Custom.t list
      }
  end
end

module Kind : sig
  type func = private
    | Wasm of
        { func : Binary.Func.t
        ; idx : int
        }
    | Extern of { idx : int }

  val wasm : Binary.Func.t -> int -> func

  val extern : int -> func

  type 'f t =
    | Wat of Text.Module.t
    | Wast of Wast.script
    | Wasm of Binary.Module.t
    | Extern of 'f Extern.Module.t
end

module Binary_to_text : sig
  (* TODO: move this to Compile.Binary.to_text *)
  val modul : Binary.Module.t -> Text.Module.t
end

module Concrete_memory : sig
  type t

  val store_8 : t -> addr:Concrete_i32.t -> Concrete_i32.t -> unit Result.t
end

module Concrete_extern_func : sig
  type _ func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func

  val extern_type : extern_func -> Text.func_type

  module Syntax : sig
    type l

    type lr

    type elt

    type mem

    type (_, _, _) t

    val ( ^-> ) : ('r, 'k, 'a) t -> 'b func_type -> ('a -> 'b) func_type

    val ( ^->. ) :
      ('r, 'k, 'a) t -> (lr, 'kk, 'b) t -> ('a -> 'b Result.t) func_type

    val ( ^->.. ) :
         ('ll, 'k, 'a) t
      -> (lr, elt, 'b1) t * (lr, elt, 'b2) t
      -> ('a -> ('b1 * 'b2) Result.t) func_type

    val ( ^->... ) :
         ('ll, 'k, 'a) t
      -> (lr, elt, 'b1) t * (lr, elt, 'b2) t * (lr, elt, 'b3) t
      -> ('a -> ('b1 * 'b2 * 'b3) Result.t) func_type

    val ( ^->.... ) :
         ('ll, 'k, 'a) t
      -> (lr, elt, 'b1) t
         * (lr, elt, 'b2) t
         * (lr, elt, 'b3) t
         * (lr, elt, 'b4) t
      -> ('a -> ('b1 * 'b2 * 'b3 * 'b4) Result.t) func_type

    val i32 : (lr, elt, Concrete_i32.t) t

    val i64 : (lr, elt, Concrete_i64.t) t

    val unit : (lr, unit, unit) t

    val memory : int -> (l, mem, Concrete_memory.t) t

    val externref : 'a Type.Id.t -> (lr, elt, 'a) t
  end
end

module Parse : sig
  module Text : sig
    module Module : sig
      val from_file : Fpath.t -> Text.Module.t Result.t

      val from_string : string -> Text.Module.t Result.t
    end
  end

  module Binary : sig
    module Module : sig
      val from_file : Fpath.t -> Binary.Module.t Result.t

      val from_string : string -> Binary.Module.t Result.t
    end
  end
end

module Label : sig
  module Coverage_criteria : sig
    type t =
      | Function_coverage
      | Statement_coverage
      | Decision_coverage

    val of_string : string -> (t, [ `Msg of string ]) Prelude.Result.t

    val pp : t Fmt.t
  end
end

module Linked : sig
  module Module : sig
    type 'f t
  end
end

module Extern : sig
  module Module : sig
    type 'f t =
      { functions : (string * 'f) list
      ; func_type : 'f -> Text.func_type
      }
  end
end

module Link : sig
  module State : sig
    type 'f t

    val empty : unit -> 'f t
  end

  module Binary : sig
    val modul :
         name:string option
      -> 'f State.t
      -> Binary.Module.t
      -> ('f Linked.Module.t * 'f State.t) Result.t
  end

  module Extern : sig
    val modul : name:string -> 'f Extern.Module.t -> 'f State.t -> 'f State.t
  end
end

module Compile : sig
  module Text : sig
    val until_binary : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

    val until_link :
         unsafe:bool
      -> name:string option
      -> 'f Link.State.t
      -> Text.Module.t
      -> ('f Linked.Module.t * 'f Link.State.t) Result.t
  end

  module Binary : sig
    val until_link :
         unsafe:bool
      -> name:string option
      -> 'f Link.State.t
      -> Binary.Module.t
      -> ('f Linked.Module.t * 'f Link.State.t) Result.t
  end
end

module Binary_validate : sig
  val modul : Binary.Module.t -> unit Result.t
end

module Text_validate : sig
  val modul : Text.Module.t -> Text.Module.t Result.t
end

module Symbolic_boolean : sig
  type t

  val pp : t Fmt.t
end

module Symbolic_i32 : sig
  type t

  val add : t -> t -> t

  val lt : t -> t -> Symbolic_boolean.t

  val pp : t Fmt.t

  val symbol : Smtml.Symbol.t -> t
end

module Symbolic_i64 : sig
  type t

  val add : t -> t -> t

  val lt : t -> t -> Symbolic_boolean.t

  val pp : t Fmt.t
end

module Symbolic_v128 : sig
  type t

  val of_i64x2 : Symbolic_i64.t -> Symbolic_i64.t -> t

  val pp : t Fmt.t
end

module Symbolic_f32 : sig
  type t

  val pp : t Fmt.t
end

module Symbolic_f64 : sig
  type t

  val of_float : float -> t

  val pp : t Fmt.t
end

module Symbolic_choice : sig
  type 'a t

  val with_new_symbol : Smtml.Ty.t -> (Smtml.Symbol.t -> 'a) -> 'a t

  val return : 'a -> 'a t

  val stop : 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val trap : Result.err -> 'a t
end

module Symbolic_extern_func : sig
  type _ func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func

  val extern_type : extern_func -> Text.func_type

  module Syntax : sig
    type l

    type lr

    type elt

    type mem

    type (_, _, _) t

    val ( ^-> ) : ('r, 'k, 'a) t -> 'b func_type -> ('a -> 'b) func_type

    val ( ^->. ) :
         ('r, 'k, 'a) t
      -> (lr, 'kk, 'b) t
      -> ('a -> 'b Symbolic_choice.t) func_type

    val ( ^->.. ) :
         ('ll, 'k, 'a) t
      -> (lr, elt, 'b1) t * (lr, elt, 'b2) t
      -> ('a -> ('b1 * 'b2) Symbolic_choice.t) func_type

    val ( ^->... ) :
         ('ll, 'k, 'a) t
      -> (lr, elt, 'b1) t * (lr, elt, 'b2) t * (lr, elt, 'b3) t
      -> ('a -> ('b1 * 'b2 * 'b3) Symbolic_choice.t) func_type

    val ( ^->.... ) :
         ('ll, 'k, 'a) t
      -> (lr, elt, 'b1) t
         * (lr, elt, 'b2) t
         * (lr, elt, 'b3) t
         * (lr, elt, 'b4) t
      -> ('a -> ('b1 * 'b2 * 'b3 * 'b4) Symbolic_choice.t) func_type

    val i32 : (lr, elt, Symbolic_i32.t) t

    val i64 : (lr, elt, Symbolic_i64.t) t

    val unit : (lr, unit, unit) t

    val memory : int -> (l, mem, Symbolic_memory.t) t

    val externref : 'a Type.Id.t -> (lr, elt, 'a) t
  end
end

module Interpret : sig
  module type Parameters = sig
    val use_ite_for_select : bool

    val throw_away_trap : bool

    val timeout : float option

    val timeout_instr : int option
  end

  module Default_parameters : Parameters

  module Concrete (_ : Parameters) : sig
    val modul :
         Concrete_extern_func.extern_func Link.State.t
      -> Concrete_extern_func.extern_func Linked.Module.t
      -> unit Result.t
  end

  module Symbolic (_ : Parameters) : sig
    val modul :
         Symbolic_extern_func.extern_func Link.State.t
      -> Symbolic_extern_func.extern_func Linked.Module.t
      -> unit Symbolic_choice.t
  end
end

module Model : sig
  type output_format =
    | Scfg
    | Json
end

module Symbolic_parameters : sig
  type fail_mode =
    | Trap_only
    | Assertion_only
    | Both

  module Exploration_strategy : sig
    type t =
      | FIFO
      | LIFO
      | Random
      | Random_unseen_then_random
      | Rarity
      | Hot_path_penalty
      | Rarity_aging
      | Rarity_depth_aging
      | Rarity_depth_loop_aging
      | Rarity_depth_loop_aging_random

    val of_string : String.t -> (t, [ `Msg of string ]) Prelude.Result.t

    val pp : t Fmt.t
  end

  type t =
    { unsafe : bool
    ; workers : int
    ; no_stop_at_failure : bool
    ; no_value : bool
    ; no_assert_failure_expression_printing : bool
    ; deterministic_result_order : bool
    ; fail_mode : fail_mode
    ; exploration_strategy : Exploration_strategy.t
    ; workspace : Fpath.t option
    ; solver : Smtml.Solver_type.t
    ; model_format : Model.output_format
    ; entry_point : string option
    ; invoke_with_symbols : bool
    ; model_out_file : Fpath.t option
    ; with_breadcrumbs : bool
    ; use_ite_for_select : bool
    }
end

module Symbolic_driver : sig
  val handle_result :
       exploration_strategy:Symbolic_parameters.Exploration_strategy.t
    -> workers:int
    -> no_stop_at_failure:bool
    -> no_value:bool
    -> no_assert_failure_expression_printing:bool
    -> deterministic_result_order:bool
    -> fail_mode:Symbolic_parameters.fail_mode
    -> workspace:Fpath.t
    -> solver:Smtml.Solver_type.t
    -> model_format:Model.output_format
    -> model_out_file:Fpath.t option
    -> with_breadcrumbs:bool
    -> run_time:float option
    -> unit Symbolic_choice.t
    -> unit Result.t
end

module Cmd_sym : sig
  val cmd :
    parameters:Symbolic_parameters.t -> source_file:Fpath.t -> unit Result.t
end

module Cmd_c : sig
  val cmd :
       symbolic_parameters:Symbolic_parameters.t
    -> arch:int
    -> property:Fpath.t option
    -> testcomp:bool
    -> opt_lvl:string
    -> includes:Fpath.t list
    -> files:Fpath.t list
    -> eacsl:bool
    -> out_file:Fpath.t option
    -> unit Result.t
end

module Cmd_call_graph : sig
  type mode =
    | Complete
    | Sound

  val cmd :
       call_graph_mode:mode
    -> source_file:Fpath.t
    -> entry_point:string option
    -> unit Result.t
end

module Cmd_cfg : sig
  val cmd : source_file:Fpath.t -> entry_point:string option -> unit Result.t
end

module Cmd_cpp : sig
  val cmd :
       symbolic_parameters:Symbolic_parameters.t
    -> arch:int
    -> opt_lvl:string
    -> includes:Fpath.t list
    -> files:Fpath.t list
    -> out_file:Fpath.t option
    -> unit Result.t
end

module Cmd_fmt : sig
  val cmd : inplace:bool -> files:Fpath.t list -> unit Result.t
end

module Cmd_instrument_label : sig
  val cmd :
       unsafe:bool
    -> coverage_criteria:Label.Coverage_criteria.t
    -> source_file:Fpath.t
    -> unit Result.t
end

module Cmd_iso : sig
  val cmd :
       deterministic_result_order:bool
    -> fail_mode:Symbolic_parameters.fail_mode
    -> exploration_strategy:Symbolic_parameters.Exploration_strategy.t
    -> files:Fpath.t list
    -> model_format:Model.output_format
    -> no_assert_failure_expression_printing:bool
    -> no_stop_at_failure:bool
    -> no_value:bool
    -> solver:Smtml.Solver_type.t
    -> unsafe:bool
    -> workers:int
    -> workspace:Fpath.t option
    -> model_out_file:Fpath.t option
    -> with_breadcrumbs:bool
    -> unit Result.t
end

module Cmd_replay : sig
  val cmd :
       unsafe:bool
    -> replay_file:Fpath.t
    -> source_file:Fpath.t
    -> entry_point:string option
    -> invoke_with_symbols:bool
    -> unit Result.t
end

module Cmd_run : sig
  val cmd :
       unsafe:bool
    -> timeout:float option
    -> timeout_instr:int option
    -> source_file:Fpath.t
    -> unit Result.t
end

module Cmd_rust : sig
  val cmd :
       symbolic_parameters:Symbolic_parameters.t
    -> arch:int
    -> opt_lvl:string
    -> includes:Fpath.t list
    -> files:Fpath.t list
    -> out_file:Fpath.t option
    -> unit Result.t
end

module Cmd_script : sig
  val cmd : files:Fpath.t list -> no_exhaustion:bool -> unit Result.t
end

module Cmd_tinygo : sig
  val cmd :
       symbolic_parameters:Symbolic_parameters.t
    -> files:Fpath.t list
    -> out_file:Fpath.t option
    -> unit Result.t
end

module Cmd_validate : sig
  val cmd : files:Fpath.t list -> unit Result.t
end

module Cmd_version : sig
  val owi_version : unit -> string

  val cmd : unit -> unit Result.t
end

module Cmd_wasm2wat : sig
  val cmd :
       source_file:Fpath.t
    -> emit_file:bool
    -> out_file:Fpath.t option
    -> unit Result.t
end

module Cmd_wat2wasm : sig
  val cmd :
       unsafe:bool
    -> out_file:Fpath.t option
    -> source_file:Fpath.t
    -> unit Result.t
end

module Cmd_zig : sig
  val cmd :
       symbolic_parameters:Symbolic_parameters.t
    -> includes:Fpath.t list
    -> files:Fpath.t list
    -> out_file:Fpath.t option
    -> unit Result.t
end

module Log : sig
  val main_src : Logs.Src.t

  val bench_src : Logs.Src.t

  val err : 'a Logs.log

  val setup :
    Fmt.style_renderer option -> Logs.level option -> bench:bool -> unit
end
