module V : Value_intf.T = struct
  type vbool = bool

  type int32 = Int32.t

  type int64 = Int64.t

  type float32 = Float32.t

  type float64 = Float64.t

  let const_i32 x = x

  let const_i64 x = x

  let const_f32 x = x

  let const_f64 x = x

  include Value

  module Bool = struct
    let not = not

    let int32 = function true -> 1l | false -> 0l
  end

  module I32 = Int32
  module I64 = Int64
  module F32 = Float32
  module F64 = Float64
end
