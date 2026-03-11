module Terms =
  Terms.Builder.Make (Terms.Condition.ConditionCudd) (Terms.Relations.Equality)
    ()

module SVA : Single_value_abstraction.Sig.NUMERIC_ENUM = struct
  include Single_value_abstraction.Ival
  include Single_value_abstraction.Bitfield
end

module NonRelationalDomain = Domains.Term_based.Nonrelational.Make (Terms) (SVA)
module ADomain = Domains.Term_domain.Make (Terms) (NonRelationalDomain)

let size32 = Units.In_bits.s32

let size64 = Units.In_bits.of_int 64

let size_equal size1 size2 = Units.In_bits.compare size1 size2 = 0
