module SVA = struct
  type i32 = Abs_value.Domain.integer

  type i64 = Abs_value.Domain.integer

  type f32

  type f64

  type v128
end

include Extern.Func.Make (SVA) (Result) (Concrete_memory)
