open Core
open Types

exception Num of num_type
exception TypeError of int * Num.t * num_type
exception DivideByZero
exception ConversionToInteger
exception IntegerOverflow

let of_arg f n v = try f v with Num t -> raise (TypeError (n, v, t))

module I32Op = struct
  open Types.I32
  open Int32

  let bitwidth = 32
  let to_value i : Num.t = I32 i

  let of_value n v : t =
    of_arg (fun v -> match v with I32 i -> i | _ -> raise (Num `I32Type)) n v

  let cmp_u x op y = op (x + min_value) (y + min_value)
  let lt_u x y = cmp_u x ( < ) y
  let le_u x y = cmp_u x ( <= ) y
  let gt_u x y = cmp_u x ( > ) y
  let ge_u x y = cmp_u x ( >= ) y

  let divrem_u n d =
    if d = zero then raise DivideByZero
    else
      let t = shift_right d (Int.( - ) bitwidth 1) in
      let n' = n land lnot t in
      let q = shift_left (shift_right_logical n' 1 / d) 1 in
      let r = n - (q * d) in
      if cmp_u r ( < ) d then (q, r) else (q + one, r - d)

  let div_u x y =
    let q, _ = divrem_u x y in
    q

  let shift f x y = f x (to_int_exn (y land of_int_exn (Int.( - ) bitwidth 1)))
  let shl x y = shift shift_left x y
  let shr_s x y = shift shift_right x y
  let shr_u x y = shift shift_right_logical x y

  let unop (op : I32.unop) : Num.t -> Num.t =
    let f =
      match op with Clz -> fun i -> of_int_trunc (clz i) | Not -> bit_not
    in
    fun v -> to_value (f (of_value 1 v))

  let binop (op : I32.binop) : Num.t -> Num.t -> Num.t =
    let f =
      match op with
      | Add -> ( + )
      | Sub -> ( - )
      | Mul -> ( * )
      | DivS -> ( / )
      | DivU -> div_u
      | RemS -> rem
      | RemU -> rem
      | And -> ( land )
      | Or -> ( lor )
      | Xor -> ( lxor )
      | Shl -> shl
      | ShrU -> shr_s
      | ShrS -> shr_u
      | Rotl | Rotr -> assert false
    in
    fun v1 v2 -> to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop op : Num.t -> Num.t -> bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | LtS -> ( < )
      | LtU -> lt_u
      | LeS -> ( >= )
      | LeU -> le_u
      | GtS -> ( > )
      | GtU -> gt_u
      | GeS -> ( >= )
      | GeU -> ge_u
    in
    fun v1 v2 -> f (of_value 1 v1) (of_value 2 v2)
end

module I64Op = struct
  open Types.I64
  open Int64

  let bitwidth = 64
  let to_value i : Num.t = I64 i

  let of_value n v : int64 =
    of_arg (fun v -> match v with I64 i -> i | _ -> raise (Num `I64Type)) n v

  let cmp_u x op y = op (x + min_value) (y + min_value)
  let lt_u x y = cmp_u x ( < ) y
  let le_u x y = cmp_u x ( <= ) y
  let gt_u x y = cmp_u x ( > ) y
  let ge_u x y = cmp_u x ( >= ) y

  let divrem_u n d =
    if d = zero then raise DivideByZero
    else
      let t = shift_right d (Int.( - ) bitwidth 1) in
      let n' = n land lnot t in
      let q = shift_left (shift_right_logical n' 1 / d) 1 in
      let r = n - (q * d) in
      if cmp_u r ( < ) d then (q, r) else (q + one, r - d)

  let div_u x y =
    let q, _ = divrem_u x y in
    q

  let shift f x y = f x (to_int_exn (y land of_int_exn (Int.( - ) bitwidth 1)))
  let shl x y = shift shift_left x y
  let shr_s x y = shift shift_right x y
  let shr_u x y = shift shift_right_logical x y

  let unop op : Num.t -> Num.t =
    let f = match op with Clz -> fun i -> of_int (clz i) | Not -> bit_not in
    fun v -> to_value (f (of_value 1 v))

  let binop op : Num.t -> Num.t -> Num.t =
    let f =
      match op with
      | Add -> ( + )
      | Sub -> ( - )
      | Mul -> ( * )
      | DivS -> ( / )
      | DivU -> div_u
      | RemS -> rem
      | RemU -> rem
      | And -> ( land )
      | Or -> ( lor )
      | Xor -> ( lxor )
      | Shl -> shl
      | ShrU -> shr_s
      | ShrS -> shr_u
      | Rotl | Rotr -> assert false
    in
    fun v1 v2 -> to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop op : Num.t -> Num.t -> bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | LtS -> ( < )
      | LtU -> lt_u
      | LeS -> ( >= )
      | LeU -> le_u
      | GtS -> ( > )
      | GtU -> gt_u
      | GeS -> ( >= )
      | GeU -> ge_u
    in
    fun v1 v2 -> f (of_value 1 v1) (of_value 2 v2)
end

module F32Op = struct
  open Types.F32
  open Float

  let to_value f : Num.t = F32 f

  let of_value =
    of_arg (fun v -> match v with F32 f -> f | _ -> raise (Num `F32Type))

  let of_float = Int32.bits_of_float
  let to_float = Int32.float_of_bits

  let unop op =
    let f =
      match op with
      | Neg -> neg
      | Abs -> abs
      | Sqrt -> sqrt
      | Nearest -> round_nearest
      | IsNan -> assert false
    in
    fun v -> to_value (of_float (f (to_float (of_value 1 v))))

  let binop op =
    let f =
      match op with
      | Add -> ( + )
      | Sub -> ( - )
      | Mul -> ( * )
      | Div -> ( / )
      | Rem -> ( % )
      | Min -> min
      | Max -> max
    in
    fun v1 v2 ->
      to_value
        (of_float (f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))))

  let relop op =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
    in
    fun v1 v2 -> f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))
end

module F64Op = struct
  open Types.F64
  open Float

  let to_value f : Num.t = F64 f

  let of_value =
    of_arg (fun v -> match v with F64 f -> f | _ -> raise (Num `F64Type))

  let of_float = Int64.bits_of_float
  let to_float = Int64.float_of_bits

  let unop op =
    let f =
      match op with
      | Neg -> neg
      | Abs -> abs
      | Sqrt -> sqrt
      | Nearest -> round_nearest
      | IsNan -> assert false
    in
    fun v -> to_value (of_float (f (to_float (of_value 1 v))))

  let binop op =
    let f =
      match op with
      | Add -> ( + )
      | Sub -> ( - )
      | Mul -> ( * )
      | Div -> ( / )
      | Rem -> ( % )
      | Min -> min
      | Max -> max
    in
    fun v1 v2 ->
      to_value
        (of_float (f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))))

  let relop op =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
    in
    fun v1 v2 -> f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))
end

module I32CvtOp = struct
  open I32
  open Int32

  let trunc_f32_s x =
    if x <> x then raise ConversionToInteger
    else
      let xf = F32Op.to_float x in
      if
        Float.(
          xf >= -Int32.(to_float min_value) || xf < Int32.(to_float min_value))
      then raise IntegerOverflow
      else F32Op.of_float xf

  let trunc_f32_u x =
    if x <> x then raise ConversionToInteger
    else
      let xf = F32Op.to_float x in
      if Float.(xf >= -.Int32.(to_float min_value) *. 2.0 || xf <= -1.0) then
        raise IntegerOverflow
      else F32Op.of_float xf

  let trunc_f64_s x =
    if Int64.( <> ) x x then raise ConversionToInteger
    else
      let xf = F64Op.to_float x in
      if
        Float.(
          xf >= -Int64.(to_float min_value) || xf < Int32.(to_float min_value))
      then raise IntegerOverflow
      else F32Op.of_float xf

  let trunc_f64_u x =
    if Int64.( <> ) x x then raise ConversionToInteger
    else
      let xf = F64Op.to_float x in
      if Float.(xf >= -Int32.(to_float min_value) *. 2.0 || xf <= -1.0) then
        raise IntegerOverflow
      else F32Op.of_float xf

  let cvtop op v : Num.t =
    match op with
    | WrapI64 -> I32 (Int64.to_int32_exn (I64Op.of_value 1 v))
    | TruncSF32 -> I32 (trunc_f32_s (F32Op.of_value 1 v))
    | TruncUF32 -> I32 (trunc_f32_u (F32Op.of_value 1 v))
    | TruncSF64 -> I32 (trunc_f64_s (F64Op.of_value 1 v))
    | TruncUF64 -> I32 (trunc_f64_u (F64Op.of_value 1 v))
    | ReinterpretFloat -> I32 (F32Op.of_value 1 v)
    | ExtendSI32 -> raise (TypeError (1, v, `I32Type))
    | ExtendUI32 -> raise (TypeError (1, v, `I32Type))
end

module I64CvtOp = struct
  open I64
  open Int64

  let extend_i32_u x = Int64.of_int32_exn x land 0x0000_0000_ffff_ffffL

  let trunc_f32_s x =
    if Int32.( <> ) x x then raise ConversionToInteger
    else
      let xf = F32Op.to_float x in
      if
        Float.(
          xf >= -Int64.(to_float min_value) || xf < Int64.(to_float min_value))
      then raise IntegerOverflow
      else F64Op.of_float xf

  let trunc_f32_u x =
    if Int32.( <> ) x x then raise ConversionToInteger
    else
      let xf = F32Op.to_float x in
      if Float.(xf >= -Int64.(to_float min_value) *. 2.0 || xf <= -1.0) then
        raise IntegerOverflow
      else if Float.(xf >= -Int64.(to_float min_value)) then
        of_float (xf -. (* TODO(ocaml-4.03): 0x1p63 *) 9223372036854775808.0)
        lxor min_value
      else F64Op.of_float xf

  let trunc_f64_s x =
    if x <> x then raise ConversionToInteger
    else
      let xf = F64Op.to_float x in
      if
        Float.(
          xf >= -Int64.(to_float min_value) || xf < Int64.(to_float min_value))
      then raise IntegerOverflow
      else F64Op.of_float xf

  let trunc_f64_u x =
    if x <> x then raise ConversionToInteger
    else
      let xf = F64Op.to_float x in
      if Float.(xf >= -Int64.(to_float min_value) *. 2.0 || xf <= -1.0) then
        raise IntegerOverflow
      else if Float.(xf >= -Int64.(to_float min_value)) then
        of_float (xf -. (* TODO(ocaml-4.03): 0x1p63 *) 9223372036854775808.0)
        lxor min_value
      else F64Op.of_float xf

  let cvtop op v : Num.t =
    match op with
    | ExtendSI32 -> I64 (of_int32_exn (I32Op.of_value 1 v))
    | ExtendUI32 -> I64 (extend_i32_u (I32Op.of_value 1 v))
    | TruncSF32 -> I64 (trunc_f32_s (F32Op.of_value 1 v))
    | TruncUF32 -> I64 (trunc_f32_u (F32Op.of_value 1 v))
    | TruncSF64 -> I64 (trunc_f64_s (F64Op.of_value 1 v))
    | TruncUF64 -> I64 (trunc_f64_u (F64Op.of_value 1 v))
    | ReinterpretFloat -> I64 (F64Op.of_value 1 v)
    | WrapI64 -> raise (TypeError (1, v, `I64Type))
end

module F32CvtOp = struct
  open F32

  let demote_f64 x =
    let xf = F64Op.to_float x in
    if Float.(xf = xf) then F32Op.of_float xf
    else
      let nan64bits = x in
      let sign_field =
        Int64.(shift_left (shift_right_logical nan64bits 63) 31)
      in
      let significand_field =
        Int64.(shift_right_logical (shift_left nan64bits 12) 41)
      in
      let fields = Int64.( lor ) sign_field significand_field in
      Int32.( lor ) 0x7fc0_0000l (Int64.to_int32_exn fields)

  let convert_i32_s x = F32Op.of_float (Int32.to_float x)

  let convert_i32_u x =
    F32Op.of_float
      Int32.(
        if x >= zero then to_float x
        else to_float (shift_right_logical x 1 lor (x land 1l)) *. 2.0)

  let convert_i64_s x =
    F32Op.of_float
      Int64.(
        if abs x < 0x10_0000_0000_0000L then to_float x
        else
          let r = if x land 0xfffL = 0L then 0L else 1L in
          to_float (shift_right x 12 lor r)
          *. (* TODO(ocaml-4.03): 0x1p12 *) 4096.0)

  let convert_i64_u x =
    F32Op.of_float
      Int64.(
        if I64Op.lt_u x 0x10_0000_0000_0000L then to_float x
        else
          let r = if x land 0xfffL = 0L then 0L else 1L in
          to_float (shift_right_logical x 12 lor r)
          *. (* TODO(ocaml-4.03): 0x1p12 *) 4096.0)

  let cvtop op v : Num.t =
    match op with
    | DemoteF64 -> F32 (demote_f64 (F64Op.of_value 1 v))
    | ConvertSI32 -> F32 (convert_i32_s (I32Op.of_value 1 v))
    | ConvertUI32 -> F32 (convert_i32_u (I32Op.of_value 1 v))
    | ConvertSI64 -> F32 (convert_i64_s (I64Op.of_value 1 v))
    | ConvertUI64 -> F32 (convert_i64_u (I64Op.of_value 1 v))
    | ReinterpretInt -> F32 (I32Op.of_value 1 v)
    | PromoteF32 -> raise (TypeError (1, v, `F32Type))
    | ToString | OfString -> assert false
end

module F64CvtOp = struct
  open F64

  let promote_f32 x =
    let xf = F32Op.to_float x in
    if Float.(xf = xf) then F64Op.of_float xf
    else
      let nan32bits = I64CvtOp.extend_i32_u x in
      let sign_field =
        Int64.(shift_left (shift_right_logical nan32bits 31) 63)
      in
      let significand_field =
        Int64.(shift_right_logical (shift_left nan32bits 41) 12)
      in
      let fields = Int64.( lor ) sign_field significand_field in
      Int64.( lor ) 0x7ff8_0000_0000_0000L fields

  let convert_i32_s x = F64Op.of_float (Int32.to_float x)

  (*
   * Unlike the other convert_u functions, the high half of the i32 range is
   * within the range where f32 can represent odd numbers, so we can't do the
   * shift. Instead, we can use int64 signed arithmetic.
   *)
  let convert_i32_u x =
    F64Op.of_float Int64.(to_float (of_int32 x land 0x0000_0000_ffff_ffffL))

  let convert_i64_s x = F64Op.of_float (Int64.to_float x)

  (*
   * Values in the low half of the int64 range can be converted with a signed
   * conversion. The high half is beyond the range where f64 can represent odd
   * numbers, so we can shift the value right, adjust the least significant
   * bit to round correctly, do a conversion, and then scale it back up.
   *)
  let convert_i64_u x =
    F64Op.of_float
      Int64.(
        if x >= zero then to_float x
        else to_float (shift_right_logical x 1 lor (x land 1L)) *. 2.0)

  let cvtop op v : Num.t =
    match op with
    | PromoteF32 -> F64 (promote_f32 (F32Op.of_value 1 v))
    | ConvertSI32 -> F64 (convert_i32_s (I32Op.of_value 1 v))
    | ConvertUI32 -> F64 (convert_i32_u (I32Op.of_value 1 v))
    | ConvertSI64 -> F64 (convert_i64_s (I64Op.of_value 1 v))
    | ConvertUI64 -> F64 (convert_i64_u (I64Op.of_value 1 v))
    | ReinterpretInt -> F64 (I64Op.of_value 1 v)
    | DemoteF64 -> raise (TypeError (1, v, `F64Type))
    | ToString | OfString -> assert false
end

(* Dispatch *)

let op i32 i64 f32 f64 = function
  | Int _ -> failwith "eval_numeric: Integer evaluations not supported"
  | Real _ -> failwith "eval_numeric: Float evaluations not supported"
  | I32 x -> i32 x
  | I64 x -> i64 x
  | F32 x -> f32 x
  | F64 x -> f64 x
  | Str _ | Bool _ -> assert false

let eval_unop = op I32Op.unop I64Op.unop F32Op.unop F64Op.unop
let eval_binop = op I32Op.binop I64Op.binop F32Op.binop F64Op.binop
let eval_relop = op I32Op.relop I64Op.relop F32Op.relop F64Op.relop
let eval_cvtop = op I32CvtOp.cvtop I64CvtOp.cvtop F32CvtOp.cvtop F64CvtOp.cvtop
