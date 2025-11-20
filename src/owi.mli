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
    | `Multiple_memories
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
    | `Unknown_annotation_clause of Sexp.t
    | `Unknown_annotation_object of Sexp.t
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
    | `Invalid_conversion_to_integer
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

  val map : ('a -> 'b) -> 'a t -> 'b t

  val iter : ('a -> Unit.t) -> 'a t -> Unit.t

  val raw : 'a t -> 'a
end

module Text : sig
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

  type nonrec table_type = limits * ref_type

  val pp_table_type : Format.formatter -> table_type -> unit

  type nonrec global_type = mut * val_type

  val pp_global_type : Format.formatter -> global_type -> unit

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

  (* TODO: func and expr should also be parametrised on block type:
   using (param_type, result_type) M.block_type before simplify and directly an indice after *)
  type func =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }

  val pp_func : Format.formatter -> func -> unit

  (* Tables & Memories *)

  type table = string option * table_type

  val pp_table : Format.formatter -> table -> unit

  (* Modules *)

  type import_desc =
    | Import_func of string option * block_type
    | Import_table of string option * table_type
    | Import_mem of string option * limits
    | Import_global of string option * global_type

  type import =
    { modul : string
        (** The name of the module from which the import is done *)
    ; name : string  (** The name of the importee in its module of origin *)
    ; desc : import_desc
        (** If this import_desc first field is Some s, the importee is made
            available under name s, else it can only be used via its numerical
            index.*)
    }

  type export_desc =
    | Export_func of indice option
    | Export_table of indice option
    | Export_mem of indice option
    | Export_global of indice option

  type export =
    { name : string
    ; desc : export_desc
    }

  type type_def = string option * func_type

  val pp_type_def : Format.formatter -> type_def -> unit

  type global =
    { typ : global_type
    ; init : expr Annotated.t
    ; id : string option
    }

  val pp_global : Format.formatter -> global -> unit

  type data_mode =
    | Data_passive
    | Data_active of indice option * expr Annotated.t

  type data =
    { id : string option
    ; init : string
    ; mode : data_mode
    }

  val pp_data : Format.formatter -> data -> unit

  type elem_mode =
    | Elem_passive
    | Elem_active of indice option * expr Annotated.t
    | Elem_declarative

  type elem =
    { id : string option
    ; typ : ref_type
    ; init : expr Annotated.t list
    ; mode : elem_mode
    }

  val pp_elem : Format.formatter -> elem -> unit

  type module_field =
    | MType of type_def
    | MGlobal of global
    | MTable of table
    | MMem of mem
    | MFunc of func
    | MElem of elem
    | MData of data
    | MStart of indice
    | MImport of import
    | MExport of export

  val pp_module_field : Format.formatter -> module_field -> unit

  type modul =
    { id : string option
    ; fields : module_field list
    }

  val pp_modul : Format.formatter -> modul -> unit
end

module Binary : sig
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

  type nonrec mem = string option * limits

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

  type nonrec table_type = limits * ref_type

  type nonrec global_type = mut * val_type

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

  (* TODO: func and expr should also be parametrised on block type:
   using (param_type, result_type) M.block_type before simplify and directly an indice after *)
  type func =
    { type_f : block_type
    ; locals : param list
    ; body : expr Annotated.t
    ; id : string option
    }

  type table = string option * table_type

  type type_def = string option * func_type

  (** export *)
  type export =
    { name : string
    ; id : int
    }

  (** named exports of a module *)
  type exports =
    { global : export list
    ; mem : export list
    ; table : export list
    ; func : export list
    }

  type global =
    { typ : global_type (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t
    ; id : string option
    }

  type data_mode =
    | Data_passive
    (* TODO: Data_active binary+const expr*)
    | Data_active of int * expr Annotated.t

  type data =
    { id : string option
    ; init : string
    ; mode : data_mode
    }

  type elem_mode =
    | Elem_passive
    (* TODO: Elem_active binary+const expr*)
    | Elem_active of int option * expr Annotated.t
    | Elem_declarative

  type elem =
    { id : string option
    ; typ : ref_type (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t list
    ; mode : elem_mode
    }

  type custom = Uninterpreted of string

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

  module Module : sig
    type t =
      { id : string option
      ; types : type_def array
      ; global : (global, global_type) Runtime.t array
      ; table : (table, table_type) Runtime.t array
      ; mem : (mem, limits) Runtime.t array
      ; func :
          (func, block_type) Runtime.t array (* TODO: switch to func_type *)
      ; elem : elem array
      ; data : data array
      ; exports : exports
      ; start : int option
      ; custom : custom list
      }
  end
end

module Binary_to_text : sig
  (* TODO: move this to Compile.Binary.to_text *)
  val modul : Binary.Module.t -> Text.modul
end

module Int32 : sig
  type t = int32

  val le : t -> t -> bool

  val add : t -> t -> t
end

module Concrete_memory : sig
  type t

  val store_8 : t -> addr:Int32.t -> Int32.t -> unit Result.t
end

module Concrete_extern_func : sig
  type _ telt =
    | I32 : Int32.t telt
    | I64 : Int64.t telt
    | F32 : Float32.t telt
    | F64 : Float64.t telt
    | V128 : V128.t telt
    | Externref : 'a Type.Id.t -> 'a telt

  type _ rtype =
    | R0 : unit rtype
    | R1 : 'a telt -> 'a rtype
    | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
    | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
    | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

  type (_, _) atype =
    | Mem : ('b, 'r) atype -> (Concrete_memory.t -> 'b, 'r) atype
    | UArg : ('b, 'r) atype -> (unit -> 'b, 'r) atype
    | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | Res : ('r, 'r) atype

  type _ func_type = Func : ('f, 'r Result.t) atype * 'r rtype -> 'f func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func

  val extern_type : extern_func -> Binary.func_type

  module Syntax : sig
    type l

    type lr

    type elt

    type mem

    type (_, _, _) t = private
      | Unit : (lr, unit, unit) t
      | Memory : (l, mem, Concrete_memory.t) t
      | Elt : 'a telt -> (lr, elt, 'a) t
      | Elt_labeled : string * 'a telt -> (l, string * elt, 'a) t

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

    val i32 : (lr, elt, Int32.t) t

    val i64 : (lr, elt, Int64.t) t

    val unit : (lr, unit, unit) t

    val memory : (l, mem, Concrete_memory.t) t

    val externref : 'a Type.Id.t -> (lr, elt, 'a) t
  end
end

module Parse : sig
  module Text : sig
    module Module : sig
      val from_file : Fpath.t -> Text.modul Result.t

      val from_string : string -> Text.modul Result.t
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

    val pp : Format.formatter -> t -> unit
  end
end

module Link : sig
  type 'f state

  val empty_state : 'f state

  type 'extern_func extern_module =
    { functions : (string * 'extern_func) list
    ; func_type : 'extern_func -> Binary.func_type
    }

  type 'a module_to_run

  val extern_module :
       Concrete_extern_func.extern_func state
    -> name:string
    -> Concrete_extern_func.extern_func extern_module
    -> Concrete_extern_func.extern_func state

  val modul :
       'f state
    -> name:string option
    -> Binary.Module.t
    -> ('f module_to_run * 'f state) Result.t
end

module Compile : sig
  module Text : sig
    val until_binary : unsafe:bool -> Text.modul -> Binary.Module.t Result.t

    val until_link :
         unsafe:bool
      -> name:string option
      -> Concrete_extern_func.extern_func Link.state
      -> Text.modul
      -> ( Concrete_extern_func.extern_func Link.module_to_run
         * Concrete_extern_func.extern_func Link.state )
         Result.t
  end
end

module Binary_validate : sig
  val modul : Binary.Module.t -> unit Result.t
end

module Interpret : sig
  module Concrete : sig
    val modul :
         timeout:float option
      -> timeout_instr:int option
      -> Concrete_extern_func.extern_func Link.state
      -> Concrete_extern_func.extern_func Link.module_to_run
      -> unit Result.t
  end
end

(* TODO: move this to a proper `Model` module ? *)
module Cmd_utils : sig
  type model_format =
    | Scfg
    | Json
end

module Cmd_sym : sig
  type fail_mode =
    | Trap_only
    | Assertion_only
    | Both

  type exploration_strategy =
    | FIFO
    | LIFO
    | Random
    | Smart

  type parameters =
    { unsafe : bool
    ; workers : int
    ; no_stop_at_failure : bool
    ; no_value : bool
    ; no_assert_failure_expression_printing : bool
    ; deterministic_result_order : bool
    ; fail_mode : fail_mode
    ; exploration_strategy : exploration_strategy
    ; workspace : Fpath.t option
    ; solver : Smtml.Solver_type.t
    ; model_format : Cmd_utils.model_format
    ; entry_point : string option
    ; invoke_with_symbols : bool
    ; model_out_file : Fpath.t option
    ; with_breadcrumbs : bool
    }

  val cmd : parameters:parameters -> source_file:Fpath.t -> unit Result.t
end

module Cmd_c : sig
  val cmd :
       symbolic_parameters:Cmd_sym.parameters
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
       symbolic_parameters:Cmd_sym.parameters
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
    -> fail_mode:Cmd_sym.fail_mode
    -> exploration_strategy:Cmd_sym.exploration_strategy
    -> files:Fpath.t list
    -> model_format:Cmd_utils.model_format
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
       symbolic_parameters:Cmd_sym.parameters
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
       symbolic_parameters:Cmd_sym.parameters
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
       symbolic_parameters:Cmd_sym.parameters
    -> includes:Fpath.t list
    -> files:Fpath.t list
    -> out_file:Fpath.t option
    -> unit Result.t
end

module Log : sig
  val err : 'a Logs.log

  val setup :
    Fmt.style_renderer option -> Logs.level option -> bench:bool -> unit
end
