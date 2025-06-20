module type M = sig
  type t

  type address

  val address : Smtml.Expr.t -> address Symbolic_choice_without_memory.t

  val address_i32 : Int32.t -> address

  val make : unit -> t

  val clone : t -> t

  val loadn : t -> address -> int -> Smtml.Expr.t

  val storen : t -> address -> Smtml.Expr.t -> int -> unit

  (** [validate_address m a range] verifies whether an operation starting at
      address [a] is valid within the address range [a] to [a + range - 1]
      (inclusive). *)
  val validate_address :
       t
    -> Smtml.Expr.t
    -> int
    -> Smtml.Expr.t Result.t Symbolic_choice_without_memory.t

  val realloc :
       t
    -> ptr:Smtml.Expr.t
    -> size:Smtml.Expr.t
    -> Smtml.Expr.t Symbolic_choice_without_memory.t

  val free : t -> Smtml.Expr.t -> Smtml.Expr.t Symbolic_choice_without_memory.t
end

module type S = sig
  type t

  type collection

  val init : unit -> collection

  val clone : collection -> collection

  val get_memory : Env_id.t -> Concrete_memory.t -> collection -> int -> t

  (* val check_within_bounds : *)
  (*   t -> Smtml.Expr.t -> (Smtml.Expr.t * Symbolic_value.int32, Trap.t) result *)

  val realloc :
       t
    -> ptr:Smtml.Expr.t
    -> size:Smtml.Expr.t
    -> Smtml.Expr.t Symbolic_choice_without_memory.t

  val free : t -> Smtml.Expr.t -> Smtml.Expr.t Symbolic_choice_without_memory.t

  val load_8_s :
    t -> Smtml.Expr.t -> Symbolic_value.int32 Symbolic_choice_without_memory.t

  val load_8_u :
    t -> Smtml.Expr.t -> Symbolic_value.int32 Symbolic_choice_without_memory.t

  val load_16_s :
    t -> Smtml.Expr.t -> Symbolic_value.int32 Symbolic_choice_without_memory.t

  val load_16_u :
    t -> Smtml.Expr.t -> Symbolic_value.int32 Symbolic_choice_without_memory.t

  val load_32 :
    t -> Smtml.Expr.t -> Symbolic_value.int32 Symbolic_choice_without_memory.t

  val load_64 :
    t -> Smtml.Expr.t -> Symbolic_value.int32 Symbolic_choice_without_memory.t

  val store_8 :
       t
    -> addr:Smtml.Expr.t
    -> Smtml.Expr.t
    -> unit Symbolic_choice_without_memory.t

  val store_16 :
       t
    -> addr:Smtml.Expr.t
    -> Smtml.Expr.t
    -> unit Symbolic_choice_without_memory.t

  val store_32 :
       t
    -> addr:Smtml.Expr.t
    -> Smtml.Expr.t
    -> unit Symbolic_choice_without_memory.t

  val store_64 :
       t
    -> addr:Smtml.Expr.t
    -> Smtml.Expr.t
    -> unit Symbolic_choice_without_memory.t

  val grow : t -> Smtml.Expr.t -> unit

  val fill : t -> pos:Smtml.Expr.t -> len:Smtml.Expr.t -> char -> unit

  val blit :
    t -> src:Smtml.Expr.t -> dst:Smtml.Expr.t -> len:Smtml.Expr.t -> unit

  val blit_string :
       t
    -> string
    -> src:Smtml.Expr.t
    -> dst:Smtml.Expr.t
    -> len:Smtml.Expr.t
    -> unit

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

module type Intf = sig
  module type M = M

  module type S = S

  module Make (_ : M) : S
end
