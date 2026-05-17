module SVA = struct
  type i32 = Abstract_value0.i32

  type i64 = Abstract_value0.i64

  type f32

  type f64

  type v128
end

include Extern.Func.Make (SVA) (Result) (Concrete_memory)
