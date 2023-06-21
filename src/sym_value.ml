open Encoding 

module Symbolic = struct
  module Expr = Expression

  type vbool = Expr.t

  type int32 = Expr.t

  type int64 = Expr.t

  type float32 = Expr.t

  type float64 = Expr.t

  type 'a ref_value = unit

  type 'a t =
    | I32 of int32
    | I64 of int64
    | F32 of float32
    | F64 of float64
    | Ref of 'a ref_value

  let mk_i32 x = Expr.Val (Value.Num (Types.I32 x))
  let mk_i64 x = Expr.Val (Value.Num (Types.I64 x))
  let mk_f32 x = Expr.Val (Value.Num (Types.F32 x))
  let mk_f64 x = Expr.Val (Value.Num (Types.F64 x))

  let const_i32 (i : Int32.t) : int32 = mk_i32 i

  let const_i64 (i : Int64.t) : int64 = mk_i64 i

  let const_f32 (f : Float32.t) : float32 = mk_f32 (Float32.to_bits f)

  let const_f64 (f : Float64.t) : float64 = mk_f64 (Float64.to_bits f)

  let ref_null _ = assert false

  let pp _ _ = failwith "TODO"

  module Bool = struct
    let not = Boolean.mk_not 
    let int32 v = Boolean.mk_ite v (mk_i32 0l) (mk_i32 1l)
  end

  module I32 = struct 
    type num = Expr 
    type vbool = Expr 
    type const = Int64.t
  end

  module I64 = struct 
    type num = Expr 
    type vbool = Expr 
    type const = Int64.t
  end



  module F32 = struct 
    type num = Expr 
    type vbool = Expr 
  end

  module F64 = struct 
    type num = Expr 
    type vbool = Expr 
  end



end



module Test : Value_intf.T = Symbolic

