module Size : sig
  val b32 : Units.In_bits.t

  val b64 : Units.In_bits.t

  val equal : Units.In_bits.t -> Units.In_bits.t -> bool
end

module ADomain : Codex.Domains.Sig.BASE_WITH_INTEGER
