include Int32

type t = int32

let wrap_i64 x = Int64.to_int32 x

let trunc_f32_s x =
  if Float32.ne x x then Error `Conversion_to_integer
  else
    let xf = Float32.to_float x in
    if
      let xf = Float64.of_float xf in
      let mif = Int32.(to_float min_int) in
      Float64.(ge xf (of_float ~-.mif)) || Float64.(lt xf (of_float mif))
    then Error `Integer_overflow
    else Ok (Int32.of_float xf)

let trunc_f32_u x =
  if Float32.ne x x then Error `Conversion_to_integer
  else
    let xf = Float32.to_float x in
    if
      let xf = Float64.of_float xf in
      Float64.(ge xf (of_float @@ (-.Int32.(to_float min_int) *. 2.0)))
      || Float64.(le xf (Float64.of_float ~-.1.0))
    then Error `Integer_overflow
    else Ok Int64.(to_int32 (of_float xf))

let trunc_f64_s x =
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int32.(to_float min_int) in
    Float64.(ge x (of_float @@ -.mif))
    || Float64.(le x (of_float @@ (mif -. 1.0)))
  then Error `Integer_overflow
  else Ok (Int32.of_float (Float64.to_float x))

let trunc_f64_u x =
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int32.to_float Int32.min_int in
    Float64.(ge x (of_float @@ (-.mif *. 2.0)))
    || Float64.(le x (of_float ~-.1.0))
  then Error `Integer_overflow
  else Ok Int64.(to_int32 (of_float (Float64.to_float x)))

let trunc_sat_f32_s x =
  if Float32.ne x x then 0l
  else
    let xf = Float32.to_float x |> Float64.of_float in
    let mif = Int32.(to_float min_int) in
    if Float64.(lt xf (of_float mif)) then Int32.min_int
    else if Float64.(ge xf (of_float ~-.mif)) then Int32.max_int
    else Int32.of_float (Float64.to_float xf)

let trunc_sat_f32_u x =
  if Float32.ne x x then 0l
  else
    let xf = Float32.to_float x |> Float64.of_float in
    if Float64.(le xf (of_float ~-.1.0)) then 0l
    else if Float64.(ge xf @@ of_float @@ (~-.Int32.(to_float min_int) *. 2.0))
    then -1l
    else Int64.(to_int32 @@ of_float (Float64.to_float xf))

let trunc_sat_f64_s x =
  if Float64.ne x x then 0l
  else if Float64.(le x @@ of_float @@ Int32.(to_float min_int)) then
    Int32.min_int
  else if Float64.(ge x @@ of_float @@ ~-.Int32.(to_float min_int)) then
    Int32.max_int
  else Int32.of_float @@ Float64.to_float x

let trunc_sat_f64_u x =
  if Float64.ne x x then 0l
  else if Float64.(le x @@ of_float ~-.1.0) then 0l
  else if Float64.(ge x @@ of_float @@ ~-.(Int32.(to_float min_int) *. 2.0))
  then -1l
  else Int64.(to_int32 (of_float @@ Float64.to_float x))

let reinterpret_f32 = Float32.to_bits

let to_boolean = function 0l -> false | _i -> true

let of_boolean = function false -> 0l | true -> 1l

let of_concrete (v : t) : t = v

let eq_concrete (v1 : t) (v2 : Int32.t) = eq v1 v2

let pp = Fmt.int32

let of_int32 (v : int32) : t = v
