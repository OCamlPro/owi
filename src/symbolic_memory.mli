type t

type collection

val init : unit -> collection

val clone : collection -> collection

val get_memory : Env_id.t -> Concrete_memory.t -> collection -> int -> t

val check_within_bounds :
     t
  -> Encoding.Expr.t
  -> (Encoding.Expr.t * Symbolic_value.S.int32, Trap.t) result

val replace_size : t -> Int32.t -> Encoding.Expr.t -> unit

val free : t -> Int32.t -> unit

val load_8_s : t -> Encoding.Expr.t -> Symbolic_value.S.int32

val load_8_u : t -> Encoding.Expr.t -> Symbolic_value.S.int32

val load_16_s : t -> Encoding.Expr.t -> Symbolic_value.S.int32

val load_16_u : t -> Encoding.Expr.t -> Symbolic_value.S.int32

val load_32 : t -> Encoding.Expr.t -> Symbolic_value.S.int32

val load_64 : t -> Encoding.Expr.t -> Symbolic_value.S.int32

val store_8 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val store_16 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val store_32 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val store_64 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val grow : t -> Encoding.Expr.t -> unit

val fill :
  t -> pos:Encoding.Expr.t -> len:Encoding.Expr.t -> char -> Encoding.Expr.t

val blit :
     t
  -> src:Encoding.Expr.t
  -> dst:Encoding.Expr.t
  -> len:Encoding.Expr.t
  -> Encoding.Expr.t

val blit_string :
     t
  -> string
  -> src:Encoding.Expr.t
  -> dst:Encoding.Expr.t
  -> len:Encoding.Expr.t
  -> Encoding.Expr.t

val size : t -> Encoding.Expr.t

val size_in_pages : t -> Encoding.Expr.t

val get_limit_max : t -> Encoding.Expr.t option

module ITbl : sig
  type 'a t

  type key

  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

val iter : (t ITbl.t -> unit) -> collection -> unit
