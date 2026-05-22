(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Terms =
  Terms.Builder.Make (Terms.Condition.ConditionCudd) (Terms.Relations.Equality)
    ()

module SVA : Single_value_abstraction.Sig.NUMERIC_ENUM = struct
  include Single_value_abstraction.Ival
  include Single_value_abstraction.Bitfield
end

module NonRelationalDomain = Domains.Term_based.Nonrelational.Make (Terms) (SVA)

include (
  Domains.Term_domain.Make (Terms) (NonRelationalDomain) :
      Codex.Domains.Sig.BASE_WITH_INTEGER )
