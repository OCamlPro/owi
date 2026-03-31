module Size : sig
  val b32 : Units.In_bits.t

  val b64 : Units.In_bits.t

  val equal : Units.In_bits.t -> Units.In_bits.t -> bool
end

module ADomain : Codex.Domains.Sig.BASE_WITH_INTEGER

type t =
  | I32 of ADomain.binary
  | I64 of ADomain.binary

val pp : ADomain.Context.t -> Format.formatter -> t -> unit

val to_binary : t -> ADomain.binary

val of_binary : Units.In_bits.t -> ADomain.binary -> t Result.t

val equal : t -> t -> bool

val binop :
     Units.In_bits.t
  -> (ADomain.binary -> ADomain.binary -> ADomain.binary)
  -> t
  -> t
  -> t Result.t
