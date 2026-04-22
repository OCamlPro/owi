let i32 () =
  let use_dict = Random.bool () in
  let n =
    if use_dict then begin
      let n = Random.int (Array.length Fuzz_dict.i32) in
      Fuzz_dict.i32.(n)
    end
    else begin
      let n = Random.bits32 () in
      Concrete_i32.of_int32 n
    end
  in
  Fuzz_state.model := Concrete_value.I32 n :: !Fuzz_state.model;
  n

let i64 () =
  let use_dict = Random.bool () in
  let n =
    if use_dict then begin
      let n = Random.int (Array.length Fuzz_dict.i64) in
      Fuzz_dict.i64.(n)
    end
    else begin
      let n = Random.bits64 () in
      Concrete_i64.of_int64 n
    end
  in
  Fuzz_state.model := Concrete_value.I64 n :: !Fuzz_state.model;
  n

let f32 () =
  let use_dict = Random.bool () in
  let n =
    if use_dict then begin
      let n = Random.int (Array.length Fuzz_dict.f32) in
      Fuzz_dict.f32.(n)
    end
    else begin
      (* TODO: avoid going through 64 bits *)
      let n = Random.bits64 () in
      let n = Int64.float_of_bits n in
      Concrete_f32.of_float n
    end
  in
  Fuzz_state.model := Concrete_value.F32 n :: !Fuzz_state.model;
  n

let f64 () =
  let use_dict = Random.bool () in
  let n =
    if use_dict then begin
      let n = Random.int (Array.length Fuzz_dict.f64) in
      Fuzz_dict.f64.(n)
    end
    else begin
      let n = Random.bits64 () in
      let n = Int64.float_of_bits n in
      Concrete_f64.of_float n
    end
  in
  Fuzz_state.model := Concrete_value.F64 n :: !Fuzz_state.model;
  n

let v128 () =
  let n1 = Random.bits64 () in
  let n2 = Random.bits64 () in
  let n = Concrete_v128.of_i64x2 n1 n2 in
  Fuzz_state.model := Concrete_value.V128 n :: !Fuzz_state.model;
  n
