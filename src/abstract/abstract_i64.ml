(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.binary

let size = Units.In_bits.of_int 64

let eq ctx i1 i2 = Abstract_domain.Binary_Forward.beq ~size ctx i1 i2

let ne ctx i1 i2 = Abstract_boolean.not ctx (eq ctx i1 i2)

let pp ctx = Abstract_domain.binary_pretty ctx ~size

let of_binary x = x

let to_binary x = x

let of_boolean ctx boolean =
  Abstract_domain.Binary_Forward.bofbool ~size ctx boolean

let to_boolean ctx x =
  let zero = Abstract_domain.Binary_Forward.biconst ~size Z.zero ctx in
  let b = Abstract_domain.Binary_Forward.beq ~size ctx x zero in
  Abstract_boolean.not ctx b

let of_int ctx i = Abstract_domain.Binary_Forward.biconst ~size (Z.of_int i) ctx

let of_int64 ctx i =
  Abstract_domain.Binary_Forward.biconst ~size (Z.of_int64 i) ctx

let zero ctx = Abstract_domain.Binary_Forward.biconst ~size Z.zero ctx

let unknown ctx = Abstract_domain.binary_unknown ~size ctx

let eqz ctx i = eq ctx (zero ctx) i

(* TODO: proper handling of overflow *)

let add ctx x1 x2 =
  let flags = Operator.Flags.Biadd.no_overflow in
  Abstract_domain.Binary_Forward.biadd ~flags ~size ctx x1 x2

let sub ctx x1 x2 =
  let flags = Operator.Flags.Bisub.no_overflow in
  Abstract_domain.Binary_Forward.bisub ~flags ~size ctx x1 x2

let mul ctx x1 x2 =
  let flags = Operator.Flags.Bimul.pack ~nsw:true ~nuw:true in
  Abstract_domain.Binary_Forward.bimul ~flags ~size ctx x1 x2

let div_s ctx x1 x2 = Abstract_domain.Binary_Forward.bisdiv ~size ctx x1 x2

let div_u ctx x1 x2 = Abstract_domain.Binary_Forward.biudiv ~size ctx x1 x2

let rem_s ctx x1 x2 = Abstract_domain.Binary_Forward.bismod ~size ctx x1 x2

let rem_u ctx x1 x2 = Abstract_domain.Binary_Forward.biumod ~size ctx x1 x2

let and_ ctx x1 x2 = Abstract_domain.Binary_Forward.band ~size ctx x1 x2

let or_ ctx x1 x2 = Abstract_domain.Binary_Forward.bor ~size ctx x1 x2

let xor ctx x1 x2 = Abstract_domain.Binary_Forward.bxor ctx ~size x1 x2

let le_s ctx x1 x2 = Abstract_domain.Binary_Forward.bisle ~size ctx x1 x2

let le_u ctx x1 x2 = Abstract_domain.Binary_Forward.biule ~size ctx x1 x2

let ge_s ctx x1 x2 = le_s ctx x2 x1

let ge_u ctx x1 x2 = le_u ctx x2 x1

let lt_s ctx x1 x2 = Abstract_boolean.not ctx (ge_s ctx x1 x2)

let lt_u ctx x1 x2 = Abstract_boolean.not ctx (ge_u ctx x1 x2)

let gt_s ctx x1 x2 = lt_s ctx x2 x1

let gt_u ctx x1 x2 = lt_u ctx x2 x1

let shl ctx x1 x2 =
  let flags = Operator.Flags.Bshl.pack ~nsw:true ~nuw:true in
  Abstract_domain.Binary_Forward.bshl ~flags ~size ctx x1 x2

let extend_s ctx n x =
  let n = Units.In_bits.of_int n in
  let value =
    Abstract_domain.Binary_Forward.bextract ~index:Units.In_bits.zero ~size:n
      ~oldsize:size ctx x
  in
  Abstract_domain.Binary_Forward.bsext ~size ~oldsize:n ctx value
