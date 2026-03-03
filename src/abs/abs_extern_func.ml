module SVA = struct
  type i32 = Abs_value.Domain.integer

  type i64 = Abs_value.Domain.integer

  type f32 = Abs_value.Domain.integer

  type f64 = Abs_value.Domain.integer

  type v128 = Abs_value.Domain.integer
end

include Extern.Func.Make (SVA) (Result) (Concrete_memory)
