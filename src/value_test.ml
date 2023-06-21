module V :
  Value_intf.T
    with type vbool = Bool.t
     and type int32 = Int32.t
     and type int64 = Int64.t
     and type float32 = Float32.t
     and type float64 = Float64.t
     and type 'a ref_value = 'a Value.ref_value
     and type 'a t = 'a Value.t = struct
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

    let and_ = (&&)

    let or_ = (||)

    let int32 = function true -> 1l | false -> 0l
  end

  module I32 = Int32
  module I64 = Int64
  module F32 = Float32
  module F64 = Float64
end

module P : Interpret_functor_intf.P = struct
  type toremove = unit

  type t = toremove

  type env = Link.Env.t

  type memory = Memory.t

  type 'env func = 'env Value.Func.t

  type 'env table = 'env Table.t

  type 'env elem = 'env Link.Env.elem

  type data = Link.Env.data

  type 'env global = 'env Global.t

  type vbool = Bool.t

  type int32 = Int32.t

  type int64 = Int64.t

  type float32 = Float32.t

  type float64 = Float64.t

  module Choice = struct
    type 'a t = 'a

    let return = Fun.id

    let bind = ( |> )

    let select = Fun.id

    let trap : Interpret_functor_intf.trap -> 'a t = function
      | Out_of_bound_memory_access ->
        raise (Types.Trap "out of bounds memory access")
  end

  module Func = Value.Func
  module Value = V
  module Global = Global
  module Memory = Memory
  module Env = Link.Env
end
