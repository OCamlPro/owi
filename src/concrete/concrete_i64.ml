include Int64

type t = int64

let extend_i32_s x = Int64.of_int32 x

let extend_i32_u x = Int64.logand (Int64.of_int32 x) 0x0000_0000_ffff_ffffL

let trunc_f32_s x =
  if Float32.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int64.(to_float min_int) in
    Float32.(ge x @@ of_float @@ ~-.mif) || Float32.(lt x @@ of_float mif)
  then Error `Integer_overflow
  else Ok (Int64.of_float @@ Float32.to_float x)

let trunc_f32_u x =
  let mif = Int64.(to_float min_int) in
  if Float32.ne x x then Error `Conversion_to_integer
  else if
    Float32.(ge x @@ of_float ~-.(mif *. 2.0))
    || Float32.(le x @@ of_float ~-.1.0)
  then Error `Integer_overflow
  else if Float32.(ge x @@ of_float ~-.mif) then
    Ok Int64.(logxor (of_float (Float32.to_float x -. 0x1p63)) min_int)
  else Ok (Int64.of_float @@ Float32.to_float x)

let trunc_f64_s x =
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int64.(to_float min_int) in
    Float64.(ge x @@ of_float ~-.mif) || Float64.(lt x @@ of_float mif)
  then Error `Integer_overflow
  else Ok (Int64.of_float @@ Float64.to_float x)

let trunc_f64_u x =
  let mif = Int64.(to_float min_int) in
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    Float64.(ge x @@ of_float (~-.mif *. 2.0))
    || Float64.(le x @@ of_float ~-.1.0)
  then Error `Integer_overflow
  else if Float64.(ge x @@ of_float ~-.mif) then
    Ok Int64.(logxor (of_float (Float64.to_float x -. 0x1p63)) min_int)
  else Ok (Int64.of_float @@ Float64.to_float x)

let trunc_sat_f32_s x =
  if Float32.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float32.(lt x @@ of_float mif) then Int64.min_int
    else if Float32.(ge x @@ of_float ~-.mif) then Int64.max_int
    else Int64.of_float (Float32.to_float x)

let trunc_sat_f32_u x =
  if Float32.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float32.(le x @@ of_float ~-.1.0) then 0L
    else if Float32.(ge x @@ of_float (~-.mif *. 2.0)) then -1L
    else if Float32.(ge x @@ of_float ~-.mif) then
      Int64.(
        logxor (of_float (Float32.to_float x -. 9223372036854775808.0)) min_int )
    else Int64.of_float @@ Float32.to_float x

let trunc_sat_f64_s x =
  if Float64.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float64.(lt x @@ of_float mif) then Int64.min_int
    else if Float64.(ge x @@ of_float ~-.mif) then Int64.max_int
    else Int64.of_float @@ Float64.to_float x

let trunc_sat_f64_u x =
  if Float64.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float64.(le x @@ of_float ~-.1.0) then 0L
    else if Float64.(ge x @@ of_float @@ (~-.mif *. 2.0)) then -1L
    else if Float64.(ge x @@ of_float ~-.mif) then
      Int64.(
        logxor (of_float (Float64.to_float x -. 9223372036854775808.0)) min_int )
    else Int64.of_float @@ Float64.to_float x

let reinterpret_f64 = Float64.to_bits

let of_concrete v = v

let eq_concrete (v1 : t) (v2 : Int64.t) = eq v1 v2

let pp = Fmt.int64

let of_int64 (v : int64) : t = v

let to_int64 (v : t) : int64 = v
