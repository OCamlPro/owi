module type M = sig
  type t

  type address

  val create : unit -> t

  val clone : t -> t

  val ptr : Smtml.Expr.t -> address

  val address : Smtml.Expr.t -> address

  val address_i32 : Int32.t -> address

  val loadn : t -> address -> int -> Smtml.Expr.t

  val storen : t -> address -> Smtml.Expr.t -> int -> unit

  val is_within_bounds :
    t -> Smtml.Expr.t -> (Smtml.Expr.t * address, Trap.t) result

  val free : t -> address -> (unit, Trap.t) result

  val realloc : t -> address -> Smtml.Expr.t -> Smtml.Expr.t
end

module type S = sig
  type t

  type address

  type collection

  val init : unit -> collection

  val clone : collection -> collection

  val get_memory : Env_id.t -> Concrete_memory.t -> collection -> int -> t

  val is_within_bounds :
    t -> Smtml.Expr.t -> (Smtml.Expr.t * address, Trap.t) result

  val ptr : Smtml.Expr.t -> address

  val address : Smtml.Expr.t -> address

  val address_i32 : Int32.t -> address

  val free : t -> address -> (unit, Trap.t) result

  val realloc : t -> address -> Smtml.Expr.t -> Smtml.Expr.t

  val load_8_s : t -> address -> Symbolic_value.int32

  val load_8_u : t -> address -> Symbolic_value.int32

  val load_16_s : t -> address -> Symbolic_value.int32

  val load_16_u : t -> address -> Symbolic_value.int32

  val load_32 : t -> address -> Symbolic_value.int32

  val load_64 : t -> address -> Symbolic_value.int32

  val store_8 : t -> addr:address -> Smtml.Expr.t -> unit

  val store_16 : t -> addr:address -> Smtml.Expr.t -> unit

  val store_32 : t -> addr:address -> Smtml.Expr.t -> unit

  val store_64 : t -> addr:address -> Smtml.Expr.t -> unit

  val grow : t -> Smtml.Expr.t -> unit

  val fill : t -> pos:Smtml.Expr.t -> len:Smtml.Expr.t -> char -> Smtml.Expr.t

  val blit :
       t
    -> src:Smtml.Expr.t
    -> dst:Smtml.Expr.t
    -> len:Smtml.Expr.t
    -> Smtml.Expr.t

  val blit_string :
       t
    -> string
    -> src:Smtml.Expr.t
    -> dst:Smtml.Expr.t
    -> len:Smtml.Expr.t
    -> Smtml.Expr.t

  val size : t -> Smtml.Expr.t

  val size_in_pages : t -> Smtml.Expr.t

  val get_limit_max : t -> Smtml.Expr.t option

  module ITbl : sig
    type 'a t

    type key

    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end

  val iter : (t ITbl.t -> unit) -> collection -> unit
end
