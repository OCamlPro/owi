type t =
  | I64x2 of int64 * int64


let of_i8x16 _ = assert false

let of_i16x8 _ = assert false

let of_i32x4 _ = assert false

let of_i64x2 a b = I64x2 (a, b)

let of_f32x4 _ = assert false

let of_f64x2 _ = assert false

let to_i64x2 t =
  match t with
  | I64x2 (a, b) -> a, b

let zero = of_i64x2 0L 0L

let pp ppf v =
  match v with
  | I64x2 (a, b) -> Fmt.pf ppf "i64x2 %Ld %Ld" a b
