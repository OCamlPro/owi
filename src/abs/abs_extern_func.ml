module SVA = struct
  type i32 = Abs_value.ADomain.binary

  type i64 = Abs_value.ADomain.binary

  type f32 = Abs_value.ADomain.binary

  type f64 = Abs_value.ADomain.binary

  type v128 = Abs_value.ADomain.binary
end

include Extern.Func.Make (SVA) (Result) (Concrete_memory)
