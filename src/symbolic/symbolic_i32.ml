(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Smtml.Typed.Bitv32

(* See: https://github.com/formalsec/smtml/pull/629 *)
(* in the future, we might change to rotate_right but it'll require to use a select_i32 (which would not cost anything in the case of an integer) ? *)
let rotate_right = ext_rotate_right

let rotate_left = ext_rotate_left

let to_boolean (e : t) : Symbolic_boolean.t =
  match Smtml.Typed.view e with
  | Val (Bitv bv) ->
    if Smtml.Bitvector.eqz bv then Symbolic_boolean.false_
    else Symbolic_boolean.true_
  | Ptr _ -> Symbolic_boolean.true_
  | Symbol { ty = Ty_bool; _ } -> Smtml.Typed.Bitv32.to_bool e
  | Cvtop (_, OfBool, cond) -> Smtml.Typed.Unsafe.wrap cond
  | _ -> Smtml.Typed.Bitv32.to_bool e

let of_boolean (e : Symbolic_boolean.t) : t =
  match Smtml.Typed.view e with
  | Val True -> one
  | Val False -> zero
  | _ -> Smtml.Typed.Bitv32.of_bool e

let boolify e =
  match Smtml.Typed.view e with
  | Val (Bitv bv) when Smtml.Bitvector.eqz bv -> Some Symbolic_boolean.false_
  | Val (Bitv bv) when Smtml.Bitvector.eq_one bv -> Some Symbolic_boolean.true_
  | Cvtop (_, OfBool, cond) -> Some (Smtml.Typed.Unsafe.wrap cond)
  | _ -> None

let logand e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.and_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logand e1 e2

let logor e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.or_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logor e1 e2

let eq_concrete (e : t) (c : Int32.t) : Symbolic_boolean.t =
  let c = of_int32 c in
  Smtml.Typed.Bitv32.eq c e

let min_int = of_int32 Int32.min_int

let eqz (v : t) = eq v zero

let ( = ) = eq

let ( + ) = add
