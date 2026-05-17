module Size = struct
  let b32 = Units.In_bits.s32

  let b64 = Units.In_bits.of_int 64

  let equal s1 s2 = Units.In_bits.compare s1 s2 = 0
end

open Abstract_value0

let pp ctx fmt = function
  | I32 b -> Fmt.pf fmt "i32 %a" (Dom.binary_pretty ctx ~size:Size.b32) b
  | I64 b -> Fmt.pf fmt "i64 %a" (Dom.binary_pretty ctx ~size:Size.b64) b

let equal v1 v2 =
  match (v1, v2) with
  | I32 v1, I32 v2 -> Dom.Binary.equal v1 v2
  | I64 v1, I64 v2 -> Dom.Binary.equal v1 v2
  | _ -> false

let to_binary = function I32 b -> b | I64 b -> b

let of_binary size binary =
  match Units.In_bits.to_int size with
  | 32 -> I32 binary
  | 64 -> I64 binary
  | _ -> assert false

let of_boolean ctx size boolean =
  let true_ = Dom.Boolean_Forward.true_ ctx in
  if Dom.Boolean.equal boolean true_ then
    match Units.In_bits.to_int size with
    | 32 -> I32 (Dom.Binary_Forward.biconst ~size (Z.of_int32 1l) ctx)
    | 64 -> I64 (Dom.Binary_Forward.biconst ~size (Z.of_int64 1L) ctx)
    | _ -> assert false
  else
    match Units.In_bits.to_int size with
    | 32 -> I32 (Dom.Binary_Forward.biconst ~size (Z.of_int32 0l) ctx)
    | 64 -> I64 (Dom.Binary_Forward.biconst ~size (Z.of_int64 0L) ctx)
    | _ -> assert false

let size_of = function I32 _ -> Size.b32 | I64 _ -> Size.b64

let to_boolean ctx x =
  let size = Size.b32 in
  let zero = Dom.Binary_Forward.biconst ~size Z.zero ctx in
  Dom.Binary_Forward.beq ~size ctx (to_binary x) zero

let top size ctx = of_binary size @@ Dom.binary_empty ~size ctx

let zero = Dom.Binary_Forward.biconst ~size:Size.b32 (Z.of_int32 0l)

module Boolean = struct
  let of_i32 : state:Abstract_state.t -> i32 -> boolean =
   fun ~state x -> to_boolean state.ctx (I32 x)

  (* val of_i32 : i32 -> t *)

  val to_i32 : t -> i32

  val _true : t

  val _false : t

  val not : boolean -> boolean
end

module I32 = struct
  type t = i32

  let size = Size.b32

  let of_concrete ctx i = Dom.Binary_Forward.biconst ~size (Z.of_int32 i) ctx

  let eqz ctx i = Dom.Binary_Forward.beq ~size ctx zero i

  let add ctx = Dom.Binary_Forward.biadd ~size ctx

  let sub ctx = Dom.Binary_Forward.bisub ~size ctx
end

module I64 = struct
  type t = i64

  let size = Size.b64

  let of_concrete ctx i = Dom.Binary_Forward.biconst ~size (Z.of_int64 i) ctx

  let eqz ctx i =
    let zero = of_concrete ctx 0L in
    Dom.Binary_Forward.beq ~size ctx zero i

  let add ctx = Dom.Binary_Forward.biadd ~size ctx

  let sub ctx = Dom.Binary_Forward.bisub ~size ctx
end
