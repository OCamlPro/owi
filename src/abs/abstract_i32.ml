module M (D : Codex.Domains.Sig.BASE_WITH_INTEGER) = struct
  type t = D.binary

  type state = { ctx : D.Context.t }

  let size = Units.In_bits.s32

  let of_boolean ctx size boolean =
    let true_ = D.Boolean_Forward.true_ ctx in
    let v = if D.Boolean.equal boolean true_ then 1l else 0l in
    D.Binary_Forward.biconst ~size (Z.of_int32 v) ctx


end
