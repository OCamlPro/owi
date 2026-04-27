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

module F32x4 = struct
  type t = f32x4
end

module F64x2 = struct
  type t = f64x2
end

module I8x16 = struct
  type t = i8x16

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

  let eq u v = map2 (fun a b -> if a = b then 0xFF else 0x00) u v

  let splat v = of_i8x16 v v v v v v v v v v v v v v v v

  let bitmask v =
    mapi (fun i lane -> if lane land 0x80 <> 0 then 1 lsl i else 0) v
    |> fold_left ( lor ) 0 |> Concrete_i32.of_int

  let add x y = map2 Concrete_i8.add x y

  let sub x y = map2 Concrete_i8.sub x y
end

module I16x8 = struct
  type t = i16x8

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

  let eq u v = map2 (fun a b -> if a = b then 0xFFFF else 0x0000) u v

  let splat v = of_i16x8 v v v v v v v v

  let bitmask v =
    mapi (fun i lane -> if lane land 0x8000 <> 0 then 1 lsl i else 0) v
    |> fold_left ( lor ) 0 |> Concrete_i32.of_int

  let add x y = map2 Concrete_i16.add x y

  let sub x y = map2 Concrete_i16.sub x y
end

module I64x2 = struct
  type t = i64x2

  let to_i32x4 (a, b) =
    let a1, a2 = Concrete_i64.to_i32x2 a in
    let b1, b2 = Concrete_i64.to_i32x2 b in
    (a1, a2, b1, b2)

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

  let eq u v = map2 (fun a b -> if Concrete_i64.eq a b then -1L else 0L) u v

  let splat v = of_i64x2 v v

  let bitmask v =
    let a, b = to_i64x2 v in
    Concrete_i32.of_int
      ( ( if Concrete_i64.ne (Concrete_i64.logand a Concrete_i64.min_int) 0L then
            1
          else 0 )
      lor
      if Concrete_i64.ne (Concrete_i64.logand b Concrete_i64.min_int) 0L then 2
      else 0 )

  let add x y = map2 Concrete_i64.add x y

  let sub x y = map2 Concrete_i64.sub x y
end

let to_i32x4 v =
  match v with
  | I32x4 (a, b, c, d) -> (a, b, c, d)
  | v -> I64x2.to_i32x4 (to_i64x2 v)

module I32x4 = struct
  type t = i32x4

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
    let a, b, c, d = to_i32x4 v in
    let r = 0l in
    let r =
      if Concrete_i32.ne (Concrete_i32.logand a Concrete_i32.min_int) 0l then
        Concrete_i32.logor r 1l
      else r
    in
    let r =
      if Concrete_i32.ne (Concrete_i32.logand b Concrete_i32.min_int) 0l then
        Concrete_i32.logor r 2l
      else r
    in
    let r =
      if Concrete_i32.ne (Concrete_i32.logand c Concrete_i32.min_int) 0l then
        Concrete_i32.logor r 4l
      else r
    in
    let r =
      if Concrete_i32.ne (Concrete_i32.logand d Concrete_i32.min_int) 0l then
        Concrete_i32.logor r 8l
      else r
    in
    r

  let add x y = map2 Concrete_i32.add x y

  let sub x y = map2 Concrete_i32.sub x y
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
