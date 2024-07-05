module type M = sig
  type t

  type address

  val create : unit -> t

  val clone : t -> t

  val address : Smtml.Expr.t -> address

  val loadn : t -> address -> int -> Smtml.Expr.t

  val storen : t -> address -> Smtml.Expr.t -> int -> unit

  (* TODO: return address instead *)
  val is_within_bounds :
    t -> Smtml.Expr.t -> (Smtml.Expr.t * Smtml.Expr.t, Trap.t) result

  val free : t -> address -> unit

  val realloc : t -> address -> Smtml.Expr.t -> unit
end

module type S = sig end
