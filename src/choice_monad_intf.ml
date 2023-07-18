
module type Base = sig
    module V : Func_intf.Value_types

    type 'a t

    val return : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val select : V.vbool -> bool t

    val select_i32 : V.int32 -> Int32.t t

    val trap : Trap.t -> 'a t
end

module type Complete = sig
  include Base

  type thread

  val with_thread : (thread -> 'b) -> 'b t
  val add_pc : V.vbool -> unit t
  val run : 'a t -> thread -> ('a * thread) list
end
