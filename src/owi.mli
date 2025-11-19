module Result : sig
  type err

  type 'a t = ('a, err) Prelude.Result.t

  val err_to_string : err -> string

  val err_to_exit_code : err -> int
end

module Text : sig
  type modul

  val pp_modul : Format.formatter -> modul -> unit
end

module Binary : sig
  module Module : sig
    type t
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

  type 'extern_func extern_module = { functions : (string * 'extern_func) list }

  type 'a module_to_run

  val extern_module :
       Concrete_extern_func.extern_func state
    -> name:string
    -> Concrete_extern_func.extern_func extern_module
    -> Concrete_extern_func.extern_func state
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
