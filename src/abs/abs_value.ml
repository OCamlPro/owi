module Terms =
  Terms.Builder.Make (Terms.Condition.ConditionCudd) (Terms.Relations.Equality)
    ()

module SVA : Single_value_abstraction.Sig.NUMERIC_ENUM = struct
  include Single_value_abstraction.Ival
  include Single_value_abstraction.Bitfield
end

module NonRelationalDomain = Domains.Term_based.Nonrelational.Make (Terms) (SVA)
module ADomain = Domains.Term_domain.Make (Terms) (NonRelationalDomain)

module Size = struct
  let b32 = Units.In_bits.s32

  let b64 = Units.In_bits.of_int 64

  let equal s1 s2 = Units.In_bits.compare s1 s2 = 0
end

type t =
  | I32 of ADomain.binary
  | I64 of ADomain.binary

let pp ctx fmt = function
  | I32 b -> Fmt.pf fmt "i32 %a" (ADomain.binary_pretty ctx ~size:Size.b32) b
  | I64 b -> Fmt.pf fmt "i64 %a" (ADomain.binary_pretty ctx ~size:Size.b64) b

let equal v1 v2 =
  match (v1, v2) with
  | I32 v1, I32 v2 -> ADomain.Binary.equal v1 v2
  | I64 v1, I64 v2 -> ADomain.Binary.equal v1 v2
  | _ -> false

let to_binary = function I32 b -> b | I64 b -> b

let of_binary size binary =
  match Units.In_bits.to_int size with
  | 32 -> Ok (I32 binary)
  | 64 -> Ok (I64 binary)
  | size -> Fmt.error_msg "Abs_value.of_binary: unsupported size %i" size

let binop size fn lhs rhs =
  let lhs, rhs = (to_binary lhs, to_binary rhs) in
  let r = fn lhs rhs in
  of_binary size r
