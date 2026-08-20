(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type f32x4 = Concrete_f32.t * Concrete_f32.t * Concrete_f32.t * Concrete_f32.t

type f64x2 = Concrete_f64.t * Concrete_f64.t

type i8x16 =
  Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t
  * Concrete_i8.t

type i16x8 =
  Concrete_i16.t
  * Concrete_i16.t
  * Concrete_i16.t
  * Concrete_i16.t
  * Concrete_i16.t
  * Concrete_i16.t
  * Concrete_i16.t
  * Concrete_i16.t

type i32x4 = Concrete_i32.t * Concrete_i32.t * Concrete_i32.t * Concrete_i32.t

type i64x2 = Concrete_i64.t * Concrete_i64.t

type t =
  | F32x4 of f32x4
  | F64x2 of f64x2
  | I8x16 of i8x16
  | I16x8 of i16x8
  | I32x4 of i32x4
  | I64x2 of i64x2

let pp ppf v =
  match v with
  | I8x16 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) ->
    Fmt.pf ppf "i8x16 %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d" a b c d e
      f g h i j k l m n o p
  | I16x8 (a, b, c, d, e, f, g, h) ->
    Fmt.pf ppf "i8x16 %d %d %d %d %d %d %d %d" a b c d e f g h
  | I32x4 (a, b, c, d) -> Fmt.pf ppf "i32x4 %ld %ld %ld %ld" a b c d
  | I64x2 (a, b) -> Fmt.pf ppf "i64x2 %Ld %Ld" a b
  | F32x4 (a, b, c, d) ->
    Fmt.pf ppf "f32x4 %a %a %a %a" Concrete_f32.pp a Concrete_f32.pp b
      Concrete_f32.pp c Concrete_f32.pp d
  | F64x2 (a, b) -> Fmt.pf ppf "f64x2 %a %a" Concrete_f64.pp a Concrete_f64.pp b

let of_concrete v = v

let of_i8x16 a b c d e f g h i j k l m n o p =
  I64x2
    (Concrete_i64.of_i8x8 a b c d e f g h, Concrete_i64.of_i8x8 i j k l m n o p)

let of_i16x8 a b c d e f g h =
  I64x2 (Concrete_i64.of_i16x4 a b c d, Concrete_i64.of_i16x4 e f g h)

let of_i32x4 a b c d = I32x4 (a, b, c, d)

let of_i64x2 a b = I64x2 (a, b)

let of_f32x4 a b c d = F32x4 (a, b, c, d)

let of_f64x2 a b = F64x2 (a, b)

let to_i64x2 = function
  | I64x2 (a, b) -> (a, b)
  | I32x4 (a, b, c, d) -> (Concrete_i64.of_i32x2 a b, Concrete_i64.of_i32x2 c d)
  | I16x8 (a, b, c, d, e, f, g, h) ->
    (Concrete_i64.of_i16x4 a b c d, Concrete_i64.of_i16x4 e f g h)
  | I8x16 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) ->
    (Concrete_i64.of_i8x8 a b c d e f g h, Concrete_i64.of_i8x8 i j k l m n o p)
  | F64x2 (a, b) -> (Concrete_f64.to_bits a, Concrete_f64.to_bits b)
  | F32x4 (a, b, c, d) ->
    let a = Concrete_f32.to_bits a in
    let b = Concrete_f32.to_bits b in
    let c = Concrete_f32.to_bits c in
    let d = Concrete_f32.to_bits d in
    (Concrete_i64.of_i32x2 a b, Concrete_i64.of_i32x2 c d)

let to_i16x8 v =
  let a, b = to_i64x2 v in
  let p0, p1 = Concrete_i64.to_i32x2 a in
  let p2, p3 = Concrete_i64.to_i32x2 b in
  let get16 x lo = (Concrete_i32.to_int x lsr lo) land 0xFFFF in
  ( get16 p0 0
  , get16 p0 16
  , get16 p1 0
  , get16 p1 16
  , get16 p2 0
  , get16 p2 16
  , get16 p3 0
  , get16 p3 16 )

let to_i8x16 v =
  let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
  let byte x lo = (x lsr lo) land 0xFF in
  ( byte a0 0
  , byte a0 8
  , byte a1 0
  , byte a1 8
  , byte a2 0
  , byte a2 8
  , byte a3 0
  , byte a3 8
  , byte a4 0
  , byte a4 8
  , byte a5 0
  , byte a5 8
  , byte a6 0
  , byte a6 8
  , byte a7 0
  , byte a7 8 )

let to_i32x4 v =
  match v with
  | I32x4 (a, b, c, d) -> (a, b, c, d)
  | v ->
    let a, b = to_i64x2 v in
    let a1, a2 = Concrete_i64.to_i32x2 a in
    let b1, b2 = Concrete_i64.to_i32x2 b in
    (a1, a2, b1, b2)

let to_f64x2 v =
  match v with
  | F64x2 (a, b) -> (a, b)
  | v ->
    let a, b = to_i64x2 v in
    (Concrete_f64.of_bits a, Concrete_f64.of_bits b)

let to_f32x4 v =
  match v with
  | F32x4 (a, b, c, d) -> (a, b, c, d)
  | v ->
    let a, b, c, d = to_i32x4 v in
    ( Concrete_f32.of_bits a
    , Concrete_f32.of_bits b
    , Concrete_f32.of_bits c
    , Concrete_f32.of_bits d )

module I8x16 = struct
  type t = i8x16

  let map f v =
    let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
      to_i8x16 v
    in
    of_i8x16 (f a0) (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8)
      (f a9) (f a10) (f a11) (f a12) (f a13) (f a14) (f a15)

  let mapi f v =
    let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
      to_i8x16 v
    in
    of_i8x16 (f 0 a0) (f 1 a1) (f 2 a2) (f 3 a3) (f 4 a4) (f 5 a5) (f 6 a6)
      (f 7 a7) (f 8 a8) (f 9 a9) (f 10 a10) (f 11 a11) (f 12 a12) (f 13 a13)
      (f 14 a14) (f 15 a15)

  let map2 f u v =
    let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
      to_i8x16 u
    in
    let b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15 =
      to_i8x16 v
    in
    of_i8x16 (f a0 b0) (f a1 b1) (f a2 b2) (f a3 b3) (f a4 b4) (f a5 b5)
      (f a6 b6) (f a7 b7) (f a8 b8) (f a9 b9) (f a10 b10) (f a11 b11)
      (f a12 b12) (f a13 b13) (f a14 b14) (f a15 b15)

  let fold_left f acc v =
    let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
      to_i8x16 v
    in
    let acc = f acc a0 in
    let acc = f acc a1 in
    let acc = f acc a2 in
    let acc = f acc a3 in
    let acc = f acc a4 in
    let acc = f acc a5 in
    let acc = f acc a6 in
    let acc = f acc a7 in
    let acc = f acc a8 in
    let acc = f acc a9 in
    let acc = f acc a10 in
    let acc = f acc a11 in
    let acc = f acc a12 in
    let acc = f acc a13 in
    let acc = f acc a14 in
    let acc = f acc a15 in
    acc

  let to_array v =
    let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
      to_i8x16 v
    in
    [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a12; a13; a14; a15 |]

  let of_array = function
    | [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a12; a13; a14; a15 |]
      ->
      of_i8x16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
    | _ -> assert false

  let get_lane i v =
    let a = to_array v in
    a.(i)

  let replace_lane i x v =
    let a = to_array v in
    a.(i) <- Concrete_i8.wrap_i32 x;
    of_array a

  let splat v =
    let v = Concrete_i8.wrap_i32 v in
    of_i8x16 v v v v v v v v v v v v v v v v

  let bitmask v =
    fold_left
      (fun (i, acc) lane ->
        let acc = if lane land 0x80 <> 0 then acc lor (1 lsl i) else acc in
        (i + 1, acc) )
      (0, 0) v
    |> snd |> Concrete_i32.of_int

  let add x y = map2 Concrete_i8.add x y

  let sub x y = map2 Concrete_i8.sub x y

  let min_s x y = map2 Concrete_i8.min_s x y

  let min_u x y = map2 Concrete_i8.min_u x y

  let max_s x y = map2 Concrete_i8.max_s x y

  let max_u x y = map2 Concrete_i8.max_u x y

  let add_sat_s x y = map2 Concrete_i8.add_sat_s x y

  let add_sat_u x y = map2 Concrete_i8.add_sat_u x y

  let sub_sat_s x y = map2 Concrete_i8.sub_sat_s x y

  let sub_sat_u x y = map2 Concrete_i8.sub_sat_u x y

  let avgr_u x y = map2 Concrete_i8.avgr_u x y

  let abs x = map Concrete_i8.abs x

  let neg x = map Concrete_i8.neg x

  let popcnt x = map Concrete_i8.popcnt x

  let cmp f = map2 (fun a b -> if f a b then 0xFF else 0)

  let eq x y = cmp Concrete_i8.eq x y

  let ne x y = cmp Concrete_i8.ne x y

  let lt_s x y = cmp Concrete_i8.lt_s x y

  let lt_u x y = cmp Concrete_i8.lt_u x y

  let gt_s x y = cmp Concrete_i8.gt_s x y

  let gt_u x y = cmp Concrete_i8.gt_u x y

  let le_s x y = cmp Concrete_i8.le_s x y

  let le_u x y = cmp Concrete_i8.le_u x y

  let ge_s x y = cmp Concrete_i8.ge_s x y

  let ge_u x y = cmp Concrete_i8.ge_u x y

  let all_true v =
    Concrete_i32.of_boolean (fold_left (fun acc x -> acc && x <> 0) true v)

  let shl s v =
    let s = Concrete_i32.to_int s land 7 in
    map (fun x -> Concrete_i8.shl x s) v

  let shr_s s v =
    let s = Concrete_i32.to_int s land 7 in
    map (fun x -> Concrete_i8.shr_s x s) v

  let shr_u s v =
    let s = Concrete_i32.to_int s land 7 in
    map (fun x -> Concrete_i8.shr_u x s) v

  let narrow_i16x8_s u v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 u in
    let b0, b1, b2, b3, b4, b5, b6, b7 = to_i16x8 v in
    of_i8x16
      (Concrete_i16.narrow_i8_s a0)
      (Concrete_i16.narrow_i8_s a1)
      (Concrete_i16.narrow_i8_s a2)
      (Concrete_i16.narrow_i8_s a3)
      (Concrete_i16.narrow_i8_s a4)
      (Concrete_i16.narrow_i8_s a5)
      (Concrete_i16.narrow_i8_s a6)
      (Concrete_i16.narrow_i8_s a7)
      (Concrete_i16.narrow_i8_s b0)
      (Concrete_i16.narrow_i8_s b1)
      (Concrete_i16.narrow_i8_s b2)
      (Concrete_i16.narrow_i8_s b3)
      (Concrete_i16.narrow_i8_s b4)
      (Concrete_i16.narrow_i8_s b5)
      (Concrete_i16.narrow_i8_s b6)
      (Concrete_i16.narrow_i8_s b7)

  let narrow_i16x8_u u v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 u in
    let b0, b1, b2, b3, b4, b5, b6, b7 = to_i16x8 v in
    of_i8x16
      (Concrete_i16.narrow_i8_u a0)
      (Concrete_i16.narrow_i8_u a1)
      (Concrete_i16.narrow_i8_u a2)
      (Concrete_i16.narrow_i8_u a3)
      (Concrete_i16.narrow_i8_u a4)
      (Concrete_i16.narrow_i8_u a5)
      (Concrete_i16.narrow_i8_u a6)
      (Concrete_i16.narrow_i8_u a7)
      (Concrete_i16.narrow_i8_u b0)
      (Concrete_i16.narrow_i8_u b1)
      (Concrete_i16.narrow_i8_u b2)
      (Concrete_i16.narrow_i8_u b3)
      (Concrete_i16.narrow_i8_u b4)
      (Concrete_i16.narrow_i8_u b5)
      (Concrete_i16.narrow_i8_u b6)
      (Concrete_i16.narrow_i8_u b7)

  let extract_lane_s i v =
    Concrete_i32.of_int (Concrete_i8.to_int_s (get_lane i v))

  let extract_lane_u i v =
    Concrete_i32.of_int (Concrete_i8.to_int_u (get_lane i v))

  let swizzle v idx =
    let src = to_array v in
    let index = to_array idx in
    Array.init 16 (fun i ->
      let j = index.(i) land 0xff in
      if j < 16 then src.(j) else 0 )
    |> of_array

  let shuffle idx u v =
    let src = Array.make 32 0 in
    Array.blit (to_array u) 0 src 0 16;
    Array.blit (to_array v) 0 src 16 16;
    Array.init 16 (fun i -> src.(idx.(i))) |> of_array

  let bitselect v1 v2 mask =
    let a = to_array v1 in
    let b = to_array v2 in
    let m = to_array mask in
    Array.init 16 (fun i ->
      a.(i) land m.(i) lor (b.(i) land (lnot m.(i) land 0xff)) )
    |> of_array
end

module I16x8 = struct
  type t = i16x8

  let map f v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
    of_i16x8 (f a0) (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7)

  let mapi f v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
    of_i16x8 (f 0 a0) (f 1 a1) (f 2 a2) (f 3 a3) (f 4 a4) (f 5 a5) (f 6 a6)
      (f 7 a7)

  let map2 f u v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 u in
    let b0, b1, b2, b3, b4, b5, b6, b7 = to_i16x8 v in
    of_i16x8 (f a0 b0) (f a1 b1) (f a2 b2) (f a3 b3) (f a4 b4) (f a5 b5)
      (f a6 b6) (f a7 b7)

  let fold_left f acc v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
    let acc = f acc a0 in
    let acc = f acc a1 in
    let acc = f acc a2 in
    let acc = f acc a3 in
    let acc = f acc a4 in
    let acc = f acc a5 in
    let acc = f acc a6 in
    let acc = f acc a7 in
    acc

  let to_array v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
    [| a0; a1; a2; a3; a4; a5; a6; a7 |]

  let of_array a = of_i16x8 a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7)

  let get_lane i v = (to_array v).(i)

  let replace_lane i x v =
    let a = to_array v in
    a.(i) <- Concrete_i16.wrap_i32 x;
    of_array a

  let cmp f x y = map2 (fun a b -> if f a b then 0xFFFF else 0) x y

  let eq u v = cmp ( = ) u v

  let splat v =
    let v = Concrete_i16.wrap_i32 v in
    of_i16x8 v v v v v v v v

  let bitmask v =
    fold_left
      (fun (i, acc) lane ->
        let acc = if lane land 0x8000 <> 0 then acc lor (1 lsl i) else acc in
        (i + 1, acc) )
      (0, 0) v
    |> snd |> Concrete_i32.of_int

  let add x y = map2 Concrete_i16.add x y

  let sub x y = map2 Concrete_i16.sub x y

  let ne x y = cmp Concrete_i16.ne x y

  let mul x y = map2 Concrete_i16.mul x y

  let abs x = map Concrete_i16.abs x

  let neg x = map Concrete_i16.neg x

  let all_true v =
    Concrete_i32.of_boolean (fold_left (fun acc x -> acc && x <> 0) true v)

  let lt_s x y = cmp Concrete_i16.lt_s x y

  let lt_u x y = cmp Concrete_i16.lt_u x y

  let gt_s x y = cmp Concrete_i16.gt_s x y

  let gt_u x y = cmp Concrete_i16.gt_u x y

  let le_s x y = cmp Concrete_i16.le_s x y

  let le_u x y = cmp Concrete_i16.le_u x y

  let ge_s x y = cmp Concrete_i16.ge_s x y

  let ge_u x y = cmp Concrete_i16.ge_u x y

  let min_s x y = map2 Concrete_i16.min_s x y

  let min_u x y = map2 Concrete_i16.min_u x y

  let max_s x y = map2 Concrete_i16.max_s x y

  let max_u x y = map2 Concrete_i16.max_u x y

  let add_sat_s x y = map2 Concrete_i16.add_sat_s x y

  let add_sat_u x y = map2 Concrete_i16.add_sat_u x y

  let sub_sat_s x y = map2 Concrete_i16.sub_sat_s x y

  let sub_sat_u x y = map2 Concrete_i16.sub_sat_u x y

  let q15mulr_sat_s x y = map2 Concrete_i16.q15mulr_sat_s x y

  let shl s v =
    let s = Concrete_i32.to_int s land 15 in
    map (fun x -> Concrete_i16.shl x s) v

  let shr_s s v =
    let s = Concrete_i32.to_int s land 15 in
    map (fun x -> Concrete_i16.shr_s x s) v

  let shr_u s v =
    let s = Concrete_i32.to_int s land 15 in
    map (fun x -> Concrete_i16.shr_u x s) v

  let avgr_u = map2 Concrete_i16.avgr_u

  let narrow_i32x4_s u v =
    let a0, a1, a2, a3 = to_i32x4 u in
    let b0, b1, b2, b3 = to_i32x4 v in
    let f = Concrete_i32.narrow_i16_s in
    of_i16x8 (f a0) (f a1) (f a2) (f a3) (f b0) (f b1) (f b2) (f b3)

  let narrow_i32x4_u u v =
    let a0, a1, a2, a3 = to_i32x4 u in
    let b0, b1, b2, b3 = to_i32x4 v in
    let f = Concrete_i32.narrow_i16_u in
    of_i16x8 (f a0) (f a1) (f a2) (f a3) (f b0) (f b1) (f b2) (f b3)

  let extend_i8x16 f v =
    let a = I8x16.to_array v in
    let a = Array.sub a 0 8 in
    let a = Array.map (fun x -> Concrete_i16.of_int (f x)) a in
    of_i16x8 a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7)

  let extend_low_i8x16_s = extend_i8x16 Concrete_i8.to_int_s

  let extend_low_i8x16_u = extend_i8x16 Concrete_i8.to_int_u

  let extend_high_i8x16_s v =
    let a = I8x16.to_array v in
    of_i16x8
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(8)))
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(9)))
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(10)))
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(11)))
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(12)))
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(13)))
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(14)))
      (Concrete_i16.of_int (Concrete_i8.to_int_s a.(15)))

  let extend_high_i8x16_u v =
    let a = I8x16.to_array v in
    of_i16x8
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(8)))
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(9)))
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(10)))
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(11)))
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(12)))
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(13)))
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(14)))
      (Concrete_i16.of_int (Concrete_i8.to_int_u a.(15)))

  let extmul_low_i8x16_s u v =
    let a = Array.map Concrete_i8.to_int_s (I8x16.to_array u) in
    let b = Array.map Concrete_i8.to_int_s (I8x16.to_array v) in
    Array.init 8 (fun i -> Concrete_i16.of_int (a.(i) * b.(i))) |> of_array

  let extmul_low_i8x16_u u v =
    let a = Array.map Concrete_i8.to_int_u (I8x16.to_array u) in
    let b = Array.map Concrete_i8.to_int_u (I8x16.to_array v) in
    Array.init 8 (fun i -> Concrete_i16.of_int (a.(i) * b.(i))) |> of_array

  let extmul_high_i8x16_s u v =
    let a = Array.map Concrete_i8.to_int_s (I8x16.to_array u) in
    let b = Array.map Concrete_i8.to_int_s (I8x16.to_array v) in
    Array.init 8 (fun i -> Concrete_i16.of_int (a.(i + 8) * b.(i + 8)))
    |> of_array

  let extmul_high_i8x16_u u v =
    let a = Array.map Concrete_i8.to_int_u (I8x16.to_array u) in
    let b = Array.map Concrete_i8.to_int_u (I8x16.to_array v) in
    Array.init 8 (fun i -> Concrete_i16.of_int @@ (a.(i + 8) * b.(i + 8)))
    |> of_array

  let extadd_pairwise_i8x16_s v =
    let a = Array.map Concrete_i8.to_int_s (I8x16.to_array v) in
    [| a.(0) + a.(1)
     ; a.(2) + a.(3)
     ; a.(4) + a.(5)
     ; a.(6) + a.(7)
     ; a.(8) + a.(9)
     ; a.(10) + a.(11)
     ; a.(12) + a.(13)
     ; a.(14) + a.(15)
    |]
    |> Array.map Concrete_i16.of_int
    |> of_array

  let extadd_pairwise_i8x16_u v =
    let a = Array.map Concrete_i8.to_int_u (I8x16.to_array v) in
    [| a.(0) + a.(1)
     ; a.(2) + a.(3)
     ; a.(4) + a.(5)
     ; a.(6) + a.(7)
     ; a.(8) + a.(9)
     ; a.(10) + a.(11)
     ; a.(12) + a.(13)
     ; a.(14) + a.(15)
    |]
    |> Array.map Concrete_i16.of_int
    |> of_array

  let extract_lane_s i v =
    Concrete_i32.of_int (Concrete_i16.to_int_s (get_lane i v))

  let extract_lane_u i v =
    Concrete_i32.of_int (Concrete_i16.to_int_u (get_lane i v))
end

module I64x2 = struct
  type t = i64x2

  let to_i32x4 = to_i32x4

  let map f v =
    let a0, a1 = to_i64x2 v in
    of_i64x2 (f a0) (f a1)

  let mapi f v =
    let a0, a1 = to_i64x2 v in
    of_i64x2 (f 0 a0) (f 1 a1)

  let map2 f u v =
    let a0, a1 = to_i64x2 u in
    let b0, b1 = to_i64x2 v in
    of_i64x2 (f a0 b0) (f a1 b1)

  let fold_left f acc v =
    let a0, a1 = to_i64x2 v in
    let acc = f acc a0 in
    let acc = f acc a1 in
    acc

  let splat v = of_i64x2 v v

  let bitmask v =
    fold_left
      (fun (i, acc) lane ->
        let acc =
          if Concrete_i64.ne (Concrete_i64.logand lane Concrete_i64.min_int) 0L
          then acc lor (1 lsl i)
          else acc
        in
        (i + 1, acc) )
      (0, 0) v
    |> snd |> Concrete_i32.of_int

  let add x y = map2 Concrete_i64.add x y

  let sub x y = map2 Concrete_i64.sub x y

  let cmp f = map2 (fun a b -> if f a b then -1L else 0L)

  let eq u v = cmp Concrete_i64.eq u v

  let ne x y = cmp Concrete_i64.ne x y

  let mul x y = map2 Concrete_i64.mul x y

  let neg x = map Concrete_i64.neg x

  let abs x = map Concrete_i64.abs x

  let all_true v =
    Concrete_i32.of_boolean
      (fold_left (fun acc x -> acc && Concrete_i64.ne x 0L) true v)

  let lt_s = cmp Concrete_i64.lt

  let gt_s x y = cmp Concrete_i64.lt y x

  let le_s x y = cmp Concrete_i64.le x y

  let ge_s x y = cmp Concrete_i64.le y x

  let shl (s : Concrete_i32.t) v =
    let s = Concrete_i64.extend_i32_u s in
    map (fun x -> Concrete_i64.shl x s) v

  let shr_s (s : Concrete_i32.t) v =
    let s = Concrete_i64.extend_i32_u s in
    map (fun x -> Concrete_i64.ashr x s) v

  let shr_u (s : Concrete_i32.t) v =
    let s = Concrete_i64.extend_i32_u s in
    map (fun x -> Concrete_i64.lshr x s) v

  let extend_low_i32x4_s v =
    let a, b, _c, _d = to_i32x4 v in
    of_i64x2 (Int64.of_int32 a) (Int64.of_int32 b)

  let extend_low_i32x4_u v =
    let a, b, _c, _d = to_i32x4 v in
    of_i64x2
      (Int64.logand (Int64.of_int32 a) 0xFFFF_FFFFL)
      (Int64.logand (Int64.of_int32 b) 0xFFFF_FFFFL)

  let extend_high_i32x4_s v =
    let _a, _b, c, d = to_i32x4 v in
    of_i64x2 (Int64.of_int32 c) (Int64.of_int32 d)

  let extend_high_i32x4_u v =
    let _a, _b, c, d = to_i32x4 v in
    of_i64x2
      (Int64.logand (Int64.of_int32 c) 0xFFFF_FFFFL)
      (Int64.logand (Int64.of_int32 d) 0xFFFF_FFFFL)

  let extmul_low_i32x4_s u v =
    let a0, a1, _a2, _a3 = to_i32x4 u in
    let b0, b1, _b2, _b3 = to_i32x4 v in
    of_i64x2
      (Int64.mul (Int64.of_int32 a0) (Int64.of_int32 b0))
      (Int64.mul (Int64.of_int32 a1) (Int64.of_int32 b1))

  let extmul_low_i32x4_u u v =
    let a0, a1, _a2, _a3 = to_i32x4 u in
    let b0, b1, _b2, _b3 = to_i32x4 v in
    let a0 = Int64.logand (Int64.of_int32 a0) 0xFFFF_FFFFL in
    let a1 = Int64.logand (Int64.of_int32 a1) 0xFFFF_FFFFL in
    let b0 = Int64.logand (Int64.of_int32 b0) 0xFFFF_FFFFL in
    let b1 = Int64.logand (Int64.of_int32 b1) 0xFFFF_FFFFL in
    of_i64x2 (Int64.mul a0 b0) (Int64.mul a1 b1)

  let extmul_high_i32x4_s u v =
    let _a0, _a1, a2, a3 = to_i32x4 u in
    let _b0, _b1, b2, b3 = to_i32x4 v in
    of_i64x2
      (Int64.mul (Int64.of_int32 a2) (Int64.of_int32 b2))
      (Int64.mul (Int64.of_int32 a3) (Int64.of_int32 b3))

  let extmul_high_i32x4_u u v =
    let _a0, _a1, a2, a3 = to_i32x4 u in
    let _b0, _b1, b2, b3 = to_i32x4 v in
    let a2 = Int64.logand (Int64.of_int32 a2) 0xFFFF_FFFFL in
    let a3 = Int64.logand (Int64.of_int32 a3) 0xFFFF_FFFFL in
    let b2 = Int64.logand (Int64.of_int32 b2) 0xFFFF_FFFFL in
    let b3 = Int64.logand (Int64.of_int32 b3) 0xFFFF_FFFFL in
    of_i64x2 (Int64.mul a2 b2) (Int64.mul a3 b3)

  let extract_lane i v =
    let a, b = to_i64x2 v in
    match i with 0 -> a | 1 -> b | _ -> assert false

  let replace_lane i x v =
    let a, b = to_i64x2 v in
    match i with 0 -> of_i64x2 x b | 1 -> of_i64x2 a x | _ -> assert false
end

module I32x4 = struct
  type t = i32x4

  let map f v =
    let a0, a1, a2, a3 = to_i32x4 v in
    of_i32x4 (f a0) (f a1) (f a2) (f a3)

  let map2 f u v =
    let a0, a1, a2, a3 = to_i32x4 u in
    let b0, b1, b2, b3 = to_i32x4 v in
    of_i32x4 (f a0 b0) (f a1 b1) (f a2 b2) (f a3 b3)

  let mapi f v =
    let a0, a1, a2, a3 = to_i32x4 v in
    of_i32x4 (f 0 a0) (f 1 a1) (f 2 a2) (f 3 a3)

  let fold_left f acc v =
    let a0, a1, a2, a3 = to_i32x4 v in
    let acc = f acc a0 in
    let acc = f acc a1 in
    let acc = f acc a2 in
    let acc = f acc a3 in
    acc

  let eq u v = map2 (fun a b -> if Concrete_i32.eq a b then -1l else 0l) u v

  let splat v = of_i32x4 v v v v

  let bitmask v =
    fold_left
      (fun (i, acc) lane ->
        let acc =
          if Concrete_i32.ne (Concrete_i32.logand lane Concrete_i32.min_int) 0l
          then acc lor (1 lsl i)
          else acc
        in
        (i + 1, acc) )
      (0, 0) v
    |> snd |> Concrete_i32.of_int

  let add x y = map2 Concrete_i32.add x y

  let sub x y = map2 Concrete_i32.sub x y

  let ne = map2 (fun a b -> if Concrete_i32.ne a b then -1l else 0l)

  let mul x y = map2 Concrete_i32.mul x y

  let neg x = map Concrete_i32.neg x

  let abs x = map Concrete_i32.abs x

  let all_true v =
    Concrete_i32.of_boolean
      (fold_left (fun acc x -> acc && Concrete_i32.ne x 0l) true v)

  let cmp f = map2 (fun a b -> if f a b then -1l else 0l)

  let lt_s x y = cmp Concrete_i32.lt x y

  let lt_u x y = cmp Concrete_i32.lt_u x y

  let gt_s x y = cmp Concrete_i32.lt y x

  let gt_u x y = cmp Concrete_i32.lt_u y x

  let le_s x y = cmp Concrete_i32.le x y

  let le_u x y = cmp Concrete_i32.le_u x y

  let ge_s x y = cmp Concrete_i32.le y x

  let ge_u x y = cmp Concrete_i32.le_u y x

  let min_s x y = map2 Concrete_i32.min_s x y

  let min_u x y = map2 Concrete_i32.min_u x y

  let max_s x y = map2 Concrete_i32.max_s x y

  let max_u x y = map2 Concrete_i32.max_u x y

  let shl s v = map (fun x -> Concrete_i32.shl x s) v

  let shr_s s v = map (fun x -> Concrete_i32.ashr x s) v

  let shr_u s v = map (fun x -> Concrete_i32.lshr x s) v

  let extend_low_i16x8_s v =
    let a, b, _c, _d, _e, _f, _g, _h = to_i16x8 v in
    of_i32x4
      (Concrete_i32.of_int (Concrete_i16.to_int_s a))
      (Concrete_i32.of_int (Concrete_i16.to_int_s b))
      (Concrete_i32.of_int (Concrete_i16.to_int_s _c))
      (Concrete_i32.of_int (Concrete_i16.to_int_s _d))

  let extend_low_i16x8_u v =
    let a, b, _c, _d, _e, _f, _g, _h = to_i16x8 v in
    of_i32x4
      (Concrete_i32.of_int (Concrete_i16.to_int_u a))
      (Concrete_i32.of_int (Concrete_i16.to_int_u b))
      (Concrete_i32.of_int (Concrete_i16.to_int_u _c))
      (Concrete_i32.of_int (Concrete_i16.to_int_u _d))

  let extend_high_i16x8_s v =
    let _a, _b, _c, _d, e, f, g, h = to_i16x8 v in
    of_i32x4
      (Concrete_i32.of_int (Concrete_i16.to_int_s e))
      (Concrete_i32.of_int (Concrete_i16.to_int_s f))
      (Concrete_i32.of_int (Concrete_i16.to_int_s g))
      (Concrete_i32.of_int (Concrete_i16.to_int_s h))

  let extend_high_i16x8_u v =
    let _a, _b, _c, _d, e, f, g, h = to_i16x8 v in
    of_i32x4
      (Concrete_i32.of_int (Concrete_i16.to_int_u e))
      (Concrete_i32.of_int (Concrete_i16.to_int_u f))
      (Concrete_i32.of_int (Concrete_i16.to_int_u g))
      (Concrete_i32.of_int (Concrete_i16.to_int_u h))

  let extmul_low_i16x8_s u v =
    let a0, a1, a2, a3, _a4, _a5, _a6, _a7 = to_i16x8 u in
    let b0, b1, b2, b3, _b4, _b5, _b6, _b7 = to_i16x8 v in
    of_i32x4
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a0))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b0)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a1))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b1)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a2))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b2)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a3))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b3)) )

  let extmul_low_i16x8_u u v =
    let a0, a1, a2, a3, _a4, _a5, _a6, _a7 = to_i16x8 u in
    let b0, b1, b2, b3, _b4, _b5, _b6, _b7 = to_i16x8 v in
    of_i32x4
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a0))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b0)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a1))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b1)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a2))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b2)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a3))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b3)) )

  let extmul_high_i16x8_s u v =
    let _a0, _a1, _a2, _a3, a4, a5, a6, a7 = to_i16x8 u in
    let _b0, _b1, _b2, _b3, b4, b5, b6, b7 = to_i16x8 v in
    of_i32x4
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a4))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b4)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a5))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b5)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a6))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b6)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_s a7))
         (Concrete_i32.of_int (Concrete_i16.to_int_s b7)) )

  let extmul_high_i16x8_u u v =
    let _a0, _a1, _a2, _a3, a4, a5, a6, a7 = to_i16x8 u in
    let _b0, _b1, _b2, _b3, b4, b5, b6, b7 = to_i16x8 v in
    of_i32x4
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a4))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b4)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a5))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b5)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a6))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b6)) )
      (Concrete_i32.mul
         (Concrete_i32.of_int (Concrete_i16.to_int_u a7))
         (Concrete_i32.of_int (Concrete_i16.to_int_u b7)) )

  let extadd_pairwise_i16x8_s v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
    of_i32x4
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_s a0))
         (Concrete_i32.of_int (Concrete_i16.to_int_s a1)) )
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_s a2))
         (Concrete_i32.of_int (Concrete_i16.to_int_s a3)) )
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_s a4))
         (Concrete_i32.of_int (Concrete_i16.to_int_s a5)) )
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_s a6))
         (Concrete_i32.of_int (Concrete_i16.to_int_s a7)) )

  let extadd_pairwise_i16x8_u v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
    of_i32x4
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_u a0))
         (Concrete_i32.of_int (Concrete_i16.to_int_u a1)) )
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_u a2))
         (Concrete_i32.of_int (Concrete_i16.to_int_u a3)) )
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_u a4))
         (Concrete_i32.of_int (Concrete_i16.to_int_u a5)) )
      (Concrete_i32.add
         (Concrete_i32.of_int (Concrete_i16.to_int_u a6))
         (Concrete_i32.of_int (Concrete_i16.to_int_u a7)) )

  let dot_i16x8_s u v =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 u in
    let b0, b1, b2, b3, b4, b5, b6, b7 = to_i16x8 v in
    of_i32x4
      (Concrete_i32.add
         (Concrete_i32.mul (Concrete_i16.to_i32_s a0) (Concrete_i16.to_i32_s b0))
         (Concrete_i32.mul (Concrete_i16.to_i32_s a1) (Concrete_i16.to_i32_s b1)) )
      (Concrete_i32.add
         (Concrete_i32.mul (Concrete_i16.to_i32_s a2) (Concrete_i16.to_i32_s b2))
         (Concrete_i32.mul (Concrete_i16.to_i32_s a3) (Concrete_i16.to_i32_s b3)) )
      (Concrete_i32.add
         (Concrete_i32.mul (Concrete_i16.to_i32_s a4) (Concrete_i16.to_i32_s b4))
         (Concrete_i32.mul (Concrete_i16.to_i32_s a5) (Concrete_i16.to_i32_s b5)) )
      (Concrete_i32.add
         (Concrete_i32.mul (Concrete_i16.to_i32_s a6) (Concrete_i16.to_i32_s b6))
         (Concrete_i32.mul (Concrete_i16.to_i32_s a7) (Concrete_i16.to_i32_s b7)) )

  let trunc_sat_f32x4_s v =
    let a, b, c, d = to_f32x4 v in
    of_i32x4
      (Concrete_i32.trunc_sat_f32_s a)
      (Concrete_i32.trunc_sat_f32_s b)
      (Concrete_i32.trunc_sat_f32_s c)
      (Concrete_i32.trunc_sat_f32_s d)

  let trunc_sat_f32x4_u v =
    let a, b, c, d = to_f32x4 v in
    of_i32x4
      (Concrete_i32.trunc_sat_f32_u a)
      (Concrete_i32.trunc_sat_f32_u b)
      (Concrete_i32.trunc_sat_f32_u c)
      (Concrete_i32.trunc_sat_f32_u d)

  let trunc_sat_f64x2_s_zero v =
    let a, b = to_f64x2 v in
    of_i32x4
      (Concrete_i32.trunc_sat_f64_s a)
      (Concrete_i32.trunc_sat_f64_s b)
      0l 0l

  let trunc_sat_f64x2_u_zero v =
    let a, b = to_f64x2 v in
    of_i32x4
      (Concrete_i32.trunc_sat_f64_u a)
      (Concrete_i32.trunc_sat_f64_u b)
      0l 0l

  let extract_lane i v =
    let a, b, c, d = to_i32x4 v in
    match i with 0 -> a | 1 -> b | 2 -> c | 3 -> d | _ -> assert false

  let replace_lane i x v =
    let a, b, c, d = to_i32x4 v in
    match i with
    | 0 -> of_i32x4 x b c d
    | 1 -> of_i32x4 a x c d
    | 2 -> of_i32x4 a b x d
    | 3 -> of_i32x4 a b c x
    | _ -> assert false
end

let zero = of_i64x2 0L 0L

let eq a b =
  let a1, a2 = to_i64x2 a in
  let b1, b2 = to_i64x2 b in
  Concrete_i64.eq a1 b1 && Concrete_i64.eq a2 b2

let logand a b =
  let a0, a1 = to_i64x2 a in
  let b0, b1 = to_i64x2 b in
  of_i64x2 (Concrete_i64.logand a0 b0) (Concrete_i64.logand a1 b1)

let logor a b =
  let a0, a1 = to_i64x2 a in
  let b0, b1 = to_i64x2 b in
  of_i64x2 (Concrete_i64.logor a0 b0) (Concrete_i64.logor a1 b1)

let any_true v =
  let a, b = to_i64x2 v in
  not (Concrete_i64.eq a 0L && Concrete_i64.eq b 0L)

let lognot v =
  let a, b = to_i64x2 v in
  of_i64x2 (Concrete_i64.lognot a) (Concrete_i64.lognot b)

let logxor a b =
  let a0, a1 = to_i64x2 a in
  let b0, b1 = to_i64x2 b in
  of_i64x2 (Concrete_i64.logxor a0 b0) (Concrete_i64.logxor a1 b1)

let andnot a b =
  let b' = lognot b in
  logand a b'

let bitselect v1 v2 mask = logor (logand v1 mask) (andnot v2 mask)

let replace_lane8 i x v =
  let a =
    let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
      to_i8x16 v
    in
    [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a12; a13; a14; a15 |]
  in
  let x = Concrete_i8.wrap_i32 x in
  a.(i) <- x;
  of_i8x16 a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7) a.(8) a.(9) a.(10)
    a.(11) a.(12) a.(13) a.(14) a.(15)

let replace_lane16 i x v =
  let a =
    let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
    [| a0; a1; a2; a3; a4; a5; a6; a7 |]
  in
  let x = Concrete_i16.wrap_i32 x in
  a.(i) <- x;
  of_i16x8 a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7)

let replace_lane32 i x v =
  let a =
    let a0, a1, a2, a3 = to_i32x4 v in
    [| a0; a1; a2; a3 |]
  in
  a.(i) <- x;
  of_i32x4 a.(0) a.(1) a.(2) a.(3)

let replace_lane64 i x v =
  let a =
    let a0, a1 = to_i64x2 v in
    [| a0; a1 |]
  in
  a.(i) <- x;
  of_i64x2 a.(0) a.(1)

let extract_lane8 i v =
  let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
    to_i8x16 v
  in
  Concrete_i32.of_int
  @@ [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a12; a13; a14; a15 |].(
     i)

let extract_lane16 i v =
  Concrete_i32.of_int
  @@
  let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
  [| a0; a1; a2; a3; a4; a5; a6; a7 |].(i)

let extract_lane32 i v =
  let a0, a1, a2, a3 = to_i32x4 v in
  [| a0; a1; a2; a3 |].(i)

let extract_lane64 i v =
  let a0, a1 = to_i64x2 v in
  match i with 0 -> a0 | 1 -> a1 | _ -> assert false

module F32x4 = struct
  type t = f32x4

  let map f v =
    let a, b, c, d = to_f32x4 v in
    of_f32x4 (f a) (f b) (f c) (f d)

  let map2 f u v =
    let a, b, c, d = to_f32x4 u in
    let e, f', g, h = to_f32x4 v in
    of_f32x4 (f a e) (f b f') (f c g) (f d h)

  let abs v = map Concrete_f32.abs v

  let neg v = map Concrete_f32.neg v

  let sqrt v = map Concrete_f32.sqrt v

  let add u v = map2 Concrete_f32.add u v

  let sub u v = map2 Concrete_f32.sub u v

  let mul u v = map2 Concrete_f32.mul u v

  let div u v = map2 Concrete_f32.div u v

  let min u v = map2 Concrete_f32.min u v

  let max u v = map2 Concrete_f32.max u v

  let pmin u v = map2 Concrete_f32.pmin u v

  let pmax u v = map2 Concrete_f32.pmax u v

  let cmp op u v =
    let a, b, c, d = to_f32x4 u in
    let e, f, g, h = to_f32x4 v in
    of_i32x4
      (if op a e then -1l else 0l)
      (if op b f then -1l else 0l)
      (if op c g then -1l else 0l)
      (if op d h then -1l else 0l)

  let eq x y = cmp Concrete_f32.eq x y

  let ne x y = cmp Concrete_f32.ne x y

  let lt x y = cmp Concrete_f32.lt x y

  let gt x y = cmp Concrete_f32.lt y x

  let le x y = cmp Concrete_f32.le x y

  let ge x y = cmp Concrete_f32.le y x

  let ceil v = map Concrete_f32.ceil v

  let floor v = map Concrete_f32.floor v

  let trunc v = map Concrete_f32.trunc v

  let nearest v = map Concrete_f32.nearest v

  let splat v = of_f32x4 v v v v

  let convert_i32x4_s v =
    let a, b, c, d = to_i32x4 v in
    of_f32x4
      (Concrete_f32.convert_i32_s a)
      (Concrete_f32.convert_i32_s b)
      (Concrete_f32.convert_i32_s c)
      (Concrete_f32.convert_i32_s d)

  let convert_i32x4_u v =
    let a, b, c, d = to_i32x4 v in
    of_f32x4
      (Concrete_f32.convert_i32_u a)
      (Concrete_f32.convert_i32_u b)
      (Concrete_f32.convert_i32_u c)
      (Concrete_f32.convert_i32_u d)

  let convert_low_i32x4_s v =
    let a, b, _, _ = to_i32x4 v in
    of_f32x4
      (Concrete_f32.convert_i32_s a)
      (Concrete_f32.convert_i32_s b)
      (Concrete_f32.of_float 0.0)
      (Concrete_f32.of_float 0.0)

  let convert_low_i32x4_u v =
    let a, b, _, _ = to_i32x4 v in
    of_f32x4
      (Concrete_f32.convert_i32_u a)
      (Concrete_f32.convert_i32_u b)
      (Concrete_f32.of_float 0.0)
      (Concrete_f32.of_float 0.0)

  let convert_high_i32x4_s v =
    let _, _, c, d = to_i32x4 v in
    of_f32x4
      (Concrete_f32.convert_i32_s c)
      (Concrete_f32.convert_i32_s d)
      (Concrete_f32.of_float 0.0)
      (Concrete_f32.of_float 0.0)

  let convert_high_i32x4_u v =
    let _, _, c, d = to_i32x4 v in
    of_f32x4
      (Concrete_f32.convert_i32_u c)
      (Concrete_f32.convert_i32_u d)
      (Concrete_f32.of_float 0.0)
      (Concrete_f32.of_float 0.0)

  let demote_f64x2_zero v =
    let a, b = to_f64x2 v in
    of_f32x4
      (Concrete_f32.demote_f64 a)
      (Concrete_f32.demote_f64 b)
      (Concrete_f32.of_float 0.0)
      (Concrete_f32.of_float 0.0)

  let extract_lane i v =
    let a, b, c, d = to_f32x4 v in
    match i with 0 -> a | 1 -> b | 2 -> c | 3 -> d | _ -> assert false

  let replace_lane i x v =
    let a, b, c, d = to_f32x4 v in
    match i with
    | 0 -> of_f32x4 x b c d
    | 1 -> of_f32x4 a x c d
    | 2 -> of_f32x4 a b x d
    | 3 -> of_f32x4 a b c x
    | _ -> assert false
end

module F64x2 = struct
  type t = f64x2

  let mapi f v =
    let a, b = to_f64x2 v in
    of_f64x2 (f 0 a) (f 1 b)

  let map f v =
    let a, b = to_f64x2 v in
    of_f64x2 (f a) (f b)

  let map2 f u v =
    let a, b = to_f64x2 u in
    let c, d = to_f64x2 v in
    of_f64x2 (f a c) (f b d)

  let abs v = map Concrete_f64.abs v

  let neg v = map Concrete_f64.neg v

  let sqrt v = map Concrete_f64.sqrt v

  let add u v = map2 Concrete_f64.add u v

  let sub u v = map2 Concrete_f64.sub u v

  let mul u v = map2 Concrete_f64.mul u v

  let div u v = map2 Concrete_f64.div u v

  let min u v = map2 Concrete_f64.min u v

  let max u v = map2 Concrete_f64.max u v

  let pmin u v = map2 Concrete_f64.pmin u v

  let pmax u v = map2 Concrete_f64.pmax u v

  let cmp f x y =
    let a0, a1 = to_f64x2 x in
    let b0, b1 = to_f64x2 y in
    of_i64x2 (if f a0 b0 then -1L else 0L) (if f a1 b1 then -1L else 0L)

  let eq x y = cmp Concrete_f64.eq x y

  let ne x y = cmp Concrete_f64.ne x y

  let lt x y = cmp Concrete_f64.lt x y

  let gt x y = cmp Concrete_f64.lt y x

  let le x y = cmp Concrete_f64.le x y

  let ge x y = cmp Concrete_f64.le y x

  let ceil v = map Concrete_f64.ceil v

  let floor v = map Concrete_f64.floor v

  let trunc v = map Concrete_f64.trunc v

  let nearest v = map Concrete_f64.nearest v

  let splat v = of_f64x2 v v

  let convert_low_i32x4_s v =
    let a, b, _, _ = to_i32x4 v in
    of_f64x2 (Concrete_f64.convert_i32_s a) (Concrete_f64.convert_i32_s b)

  let convert_low_i32x4_u v =
    let a, b, _, _ = to_i32x4 v in
    of_f64x2 (Concrete_f64.convert_i32_u a) (Concrete_f64.convert_i32_u b)

  let convert_high_i32x4_s v =
    let _, _, c, d = to_i32x4 v in
    of_f64x2 (Concrete_f64.convert_i32_s c) (Concrete_f64.convert_i32_s d)

  let convert_high_i32x4_u v =
    let _, _, c, d = to_i32x4 v in
    of_f64x2 (Concrete_f64.convert_i32_u c) (Concrete_f64.convert_i32_u d)

  let promote_low_f32x4 v =
    let a, b, _, _ = to_f32x4 v in
    of_f64x2 (Concrete_f64.promote_f32 a) (Concrete_f64.promote_f32 b)

  let extract_lane i v =
    let a, b = to_f64x2 v in
    match i with 0 -> a | 1 -> b | _ -> assert false

  let replace_lane i x v =
    let a, b = to_f64x2 v in
    match i with 0 -> of_f64x2 x b | 1 -> of_f64x2 a x | _ -> assert false
end
