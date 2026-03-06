module Terms =
  Terms.Builder.Make (Terms.Condition.ConditionCudd) (Terms.Relations.Equality)
    ()

module SVA : Single_value_abstraction.Sig.NUMERIC_ENUM = struct
  include Single_value_abstraction.Ival
  include Single_value_abstraction.Bitfield
end

module NonRelationalDomain = Domains.Term_based.Nonrelational.Make (Terms) (SVA)
module AbsDomain = Domains.Term_domain.Make (Terms) (NonRelationalDomain)

type t = AbsDomain.binary

module Context = AbsDomain.Context

let pp ctx = AbsDomain.binary_pretty ~size:Units.In_bits.s32 ctx
