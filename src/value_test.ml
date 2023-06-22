module V :
  Value_intf.T
    with type vbool = Bool.t
     and type int32 = Int32.t
     and type int64 = Int64.t
     and type float32 = Float32.t
     and type float64 = Float64.t
     and type ref_value = Value.ref_value
     and type t = Value.t = struct
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

    let and_ = ( && )

    let or_ = ( || )

    let int32 = function true -> 1l | false -> 0l
  end

  module I32 = struct
    include Int32
    include Convert.Int32

    let to_bool i = not (Int32.eq_const i 0l)
  end

  module I64 = struct
    include Int64
    include Convert.Int64
  end

  module F32 = struct
    include Float32
    include Convert.Float32
  end

  module F64 = struct
    include Float64
    include Convert.Float64
  end
end

module P = struct
  type thread = unit

  type env = Link.Env.t

  type memory = Memory.t

  type func = Value.Func.t

  type table = Table.t

  type elem = Link.Env.elem

  type data = Link.Env.data

  type global = Global.t

  type vbool = Bool.t

  type int32 = Int32.t

  type int64 = Int64.t

  type float32 = Float32.t

  type float64 = Float64.t

  type extern_func = Value.Func.extern_func

  module Choice = struct
    type 'a t = 'a

    let return = Fun.id

    let bind = ( |> )

    let select b = b

    let select_i32 i = i

    let get = ()

    let trap msg = raise (Types.Trap msg)

    let trap : Interpret_functor_intf.trap -> 'a t = function
      | Out_of_bound_memory_access -> trap "out of bounds memory access"
      | Integer_overflow -> trap "integer overflow"
      | Integer_divide_by_zero -> trap "integer divide by zero"
  end

  module Extern_func = Value.Func
  module Value = V
  module Global = Global
  module Table = Table
  module Memory = Memory
  module Env = Link.Env

  module Module_to_run = struct
    (** runnable module *)
    type t = Link.module_to_run

    let env (t : Link.module_to_run) = t.env

    let modul (t : Link.module_to_run) = t.modul

    let to_run (t : Link.module_to_run) = t.to_run
  end
end

module P' : Interpret_functor_intf.P = P
