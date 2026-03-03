module Terms =
  Terms.Builder.Make (Terms.Condition.ConditionCudd) (Terms.Relations.Equality)
    ()

module SVA : Single_value_abstraction.Sig.NUMERIC_ENUM = struct
  include Single_value_abstraction.Ival
  include Single_value_abstraction.Bitfield
end

module NonRelationalDomain = Domains.Term_based.Nonrelational.Make (Terms) (SVA)
module Domain = Domains.Term_domain.Make (Terms) (NonRelationalDomain)
