module type P = sig
  type t

  type env

  type memory

  type func

  type table

  type 'env elem

  type data

  type 'env global

  type vbool
  type int32
  type int64
  type float32
  type float64

  module Choice : sig
    type 'a t

    val return : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val select : vbool -> bool t
  end

  module Func : Func_intf.T
    with type int32 := int32
     and type int64 := int64
     and type float32 := float32
     and type float64 := float64

  module Value : Value_intf.T with type vbool = vbool
                               and type int32 = int32
     and type int64 = int64
     and type float32 = float32
     and type float64 = float64

  module Global : sig
    type 'env t = 'env global

    val value : 'env global -> 'env Value.t

    val set_value : 'env global -> 'env Value.t -> unit

    val mut : 'env global -> Types.mut

    val typ : 'env global -> Simplified.val_type
  end

  module Memory : sig
    type t = memory

    val load_8_s : t -> Value.int32 -> Value.int32

    val load_8_u : t -> Value.int32 -> Value.int32

    val store_8 : t -> addr:Value.int32 -> Value.int32 -> unit

    (* val get_data : t -> bytes *)
    val size_in_pages : t -> Value.int32

    val get_limit_max : t -> int option

    val get_limits : t -> Types.limits
    (* val update_memory : t -> bytes -> unit *)
  end

  module Env : sig
    type t = env

    type t' = t Lazy.t

    val get_memory : t -> int -> Memory.t Result.t

    val get_func : t -> int -> t' Func.t Result.t

    val get_table : t -> int -> t' Table.t Result.t

    val get_elem : t -> int -> t' elem Result.t

    val get_data : t -> int -> data Result.t

    val get_global : t -> int -> t' Global.t Result.t

    val drop_elem : 'a elem -> unit

    val drop_data : data -> unit

    val pp : Format.formatter -> t -> unit
  end
end

module type S = sig
  (** Module to interpret a linked module. *)

  (* (\** interpret a module *\) *)
  (* val modul : Link.module_to_run -> (unit, string) result *)

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
