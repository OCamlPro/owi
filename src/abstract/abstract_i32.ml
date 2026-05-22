(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.binary

let size = Units.In_bits.s32

let pp ctx = Abstract_domain.binary_pretty ctx ~size

let of_binary x = x

let to_binary x = x

let of_boolean ctx boolean =
  let true_ = Abstract_boolean.true_ ctx in
  let n = if Abstract_boolean.equal boolean true_ then 1 else 0 in
  Abstract_domain.Binary_Forward.biconst ~size (Z.of_int n) ctx

let to_boolean _ = assert false

let of_int32 ctx i =
  Abstract_domain.Binary_Forward.biconst ~size (Z.of_int32 i) ctx

let of_int ctx i = Abstract_domain.Binary_Forward.biconst ~size (Z.of_int i) ctx

let zero ctx = Abstract_domain.Binary_Forward.biconst ~size Z.zero ctx

let unknown ctx = Abstract_domain.binary_unknown ~size ctx

let equal i1 i2 = Abstract_domain.Binary.equal i1 i2

let eq ctx i1 i2 = Abstract_domain.Binary_Forward.beq ~size ctx i1 i2

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

let and_ ctx x1 x2 = Abstract_domain.Binary_Forward.band ~size ctx x1 x2

let or_ ctx x1 x2 = Abstract_domain.Binary_Forward.bor ~size ctx x1 x2

let le_s ctx x1 x2 = Abstract_domain.Binary_Forward.bisle ~size ctx x1 x2

let le_u ctx x1 x2 = Abstract_domain.Binary_Forward.biule ~size ctx x1 x2

(* TODO: no lt function ?*)
let lt_s _ctx _x1 _x2 = assert false

(* TODO: no lt function ?*)
let lt_u _ctx _x1 _x2 = assert false
