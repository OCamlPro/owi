type trap =
  | Out_of_bound_memory_access
  | Integer_overflow
  | Integer_divide_by_zero

module type Memory_data = sig
  type int32

  type int64

  type t

  val load_8_s : t -> int32 -> int32

  val load_8_u : t -> int32 -> int32

  val load_16_s : t -> int32 -> int32

  val load_16_u : t -> int32 -> int32

  val load_32 : t -> int32 -> int32

  val load_64 : t -> int32 -> int64

  val store_8 : t -> addr:int32 -> int32 -> unit

  val store_16 : t -> addr:int32 -> int32 -> unit

  val store_32 : t -> addr:int32 -> int32 -> unit

  val store_64 : t -> addr:int32 -> int64 -> unit

  val create : Int32.t -> t

  val grow : t -> int32 -> t

  val size : t -> int32

  val size_in_pages : t -> int32
end

module type P = sig
  type thread

  type env

  type memory

  type func

  type table

  type elem

  type data

  type global

  type vbool

  type int32

  type int64

  type float32

  type float64

  type extern_func

  module Choice : sig
    type 'a t

    val return : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val get : thread t

    val select : vbool -> bool t

    val select_i32 : int32 -> Int32.t t

    val trap : trap -> 'a t
  end

  module Extern_func :
    Func_intf.T_Extern_func
      with type int32 := int32
       and type int64 := int64
       and type float32 := float32
       and type float64 := float64

  module Value :
    Value_intf.T
      with type vbool = vbool
       and type int32 = int32
       and type int64 = int64
       and type float32 = float32
       and type float64 = float64

  module Global : sig
    type t = global

    val value : global -> Value.t

    val set_value : global -> Value.t -> unit

    val mut : global -> Types.mut

    val typ : global -> Simplified.val_type
  end

  module Table : sig
    type t = table

    val get : t -> int32 -> Value.ref_value

    val set : t -> int32 -> Value.ref_value -> unit

    val size : t -> int32
  end

  module Memory : sig
    type t

    val load_8_s : t -> int32 -> int32

    val load_8_u : t -> int32 -> int32

    val load_16_s : t -> int32 -> int32

    val load_16_u : t -> int32 -> int32

    val load_32 : t -> int32 -> int32

    val load_64 : t -> int32 -> int64

    val store_8 : t -> addr:int32 -> int32 -> unit

    val store_16 : t -> addr:int32 -> int32 -> unit

    val store_32 : t -> addr:int32 -> int32 -> unit

    val store_64 : t -> addr:int32 -> int64 -> unit

    val grow : t -> int32 -> unit

    val size : t -> int32

    val size_in_pages : t -> int32
  end

  module Env : sig
    type t = env

    type t' = Env_id.t

    val get_memory : t -> int -> Memory.t Result.t

    val get_func : t -> int -> Func_intf.t Result.t

    val get_table : t -> int -> Table.t Result.t

    val get_elem : t -> int -> elem Result.t

    val get_data : t -> int -> data Result.t

    val get_global : t -> int -> Global.t Result.t

    val drop_elem : elem -> unit

    val drop_data : data -> unit

    val pp : Format.formatter -> t -> unit
  end

  module Module_to_run : sig
    (** runnable module *)
    type t

    val env : t -> env

    val to_run : t -> Simplified.expr list

    val modul : t -> Simplified.modul
  end
end

module type S = sig
  (** Module to interpret a linked module. *)
  type thread

  type env

  type 'a choice

  type module_to_run

  (** interpret a module *)
  val modul : env Env_id.collection -> module_to_run -> (unit, 'a) result choice

  (* (\** interpret a function with a given input stack and produce a new stack*\) *)
  (* val exec_vfunc : *)
  (*      Link.Env.t' Stack.t *)
  (*   -> Link.Env.t' Value.Func.t *)
  (*   -> (Link.Env.t' Stack.t, string) result *)

  (* val exec_iunop : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.iunop -> Link.Env.t' Stack.t *)

  (* val exec_funop : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.funop -> Link.Env.t' Stack.t *)

  (* val exec_ibinop : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.ibinop -> Link.Env.t' Stack.t *)

  (* val exec_fbinop : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.fbinop -> Link.Env.t' Stack.t *)

  (* val exec_itestop : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.itestop -> Link.Env.t' Stack.t *)

  (* val exec_irelop : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.irelop -> Link.Env.t' Stack.t *)

  (* val exec_frelop : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.frelop -> Link.Env.t' Stack.t *)

  (* val exec_itruncf : *)
  (*      Link.Env.t' Stack.t *)
  (*   -> Types.nn *)
  (*   -> Types.nn *)
  (*   -> Types.sx *)
  (*   -> Link.Env.t' Stack.t *)

  (* val exec_itruncsatf : *)
  (*      Link.Env.t' Stack.t *)
  (*   -> Types.nn *)
  (*   -> Types.nn *)
  (*   -> Types.sx *)
  (*   -> Link.Env.t' Stack.t *)

  (* val exec_fconverti : *)
  (*      Link.Env.t' Stack.t *)
  (*   -> Types.nn *)
  (*   -> Types.nn *)
  (*   -> Types.sx *)
  (*   -> Link.Env.t' Stack.t *)

  (* val exec_ireinterpretf : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.nn -> Link.Env.t' Stack.t *)

  (* val exec_freinterpreti : *)
  (*   Link.Env.t' Stack.t -> Types.nn -> Types.nn -> Link.Env.t' Stack.t *)
end
