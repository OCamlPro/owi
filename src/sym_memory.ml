module Intf = Interpret_functor_intf
module Value = Sym_value.S

let ( let* ) o f = Option.bind o f

let ( let+ ) o f = Option.map f o

let return x = Option.Some x

module M = struct
  module Expr = Encoding.Expression
  open Expr

  let page_size = 65_536

  type int32 = Expr.t

  type int64 = Expr.t

  type t =
    { data : (Int32.t, Expr.t) Hashtbl.t
    ; parent : t Option.t
    ; mutable limits : Types.limits
    }

  let create size =
    let limits = Types.{ min = Int32.to_int size; max = None } in
    { data = Hashtbl.create 128; parent = None; limits }

  let concretize a =
    print_endline (Expr.to_string a);
    match a with Val (Num (I32 i)) -> i | _ -> assert false

  let grow m delta =
    let delta = Int32.to_int @@ concretize delta in
    let old_size = m.limits.min in
    let limits =
      { m.limits with min = max m.limits.min (old_size + delta) }
    in
    m.limits <- limits

  let fill _ = assert false

  let blit _ = assert false

  let size { limits; _ } =
    Value.const_i32 @@ Int32.of_int @@ (limits.min * page_size)

  let size_in_pages { limits; _ } =
    Value.const_i32 @@ Int32.of_int @@ limits.min

  let clone m = { data = Hashtbl.create 0; parent = Some m; limits = m.limits }

  let rec load_byte_opt a m =
    match Hashtbl.find_opt m.data a with
    | Some b -> Some b
    | None -> Option.bind m.parent (load_byte_opt a)

  let load_byte m a = Option.value (load_byte_opt a m) ~default:Value.I32.zero

  let concat a b offset =
    match (a, b) with
    | Val (Num (I32 i1)), Val (Num (I32 i2)) ->
      let offset = Int32.of_int @@ (offset * 8) in
      Value.const_i32 (Int32.logor (Int32.shl i1 offset) i2)
    | Extract (e1, h, m1), Extract (e2, m2, l) when m1 = m2 && Expr.equal e1 e2
      ->
      Extract (e1, h, l)
    | a', b' -> Concat (a', b')

  let loadn m a n : int32 =
    let rec loop addr n i v =
      if i = n then v
      else
        let addr' = Int32.add addr @@ Int32.of_int i in
        let byte = load_byte m addr' in
        loop addr n (i + 1) (concat byte v i)
    in
    let v0 = load_byte m a in
    loop a n 1 v0

  let load_8_s m a =
    match loadn m (concretize a) 1 with
    | Val (Num (I32 i8)) -> Value.const_i32 (Int32.extend_s 8 i8)
    | e -> Binop (I32 ExtendS, Value.const_i32 24l, e)

  let load_8_u m a =
    match loadn m (concretize a) 1 with
    | Val (Num (I32 _)) as i8 -> i8
    | e -> Binop (I32 ExtendU, Value.const_i32 24l, e)

  let load_16_s m a =
    match loadn m (concretize a) 2 with
    | Val (Num (I32 i16)) -> Value.const_i32 (Int32.extend_s 16 i16)
    | e -> Binop (I32 ExtendS, Value.const_i32 16l, e)

  let load_16_u m a =
    match loadn m (concretize a) 2 with
    | Val (Num (I32 _)) as i16 -> i16
    | e -> Binop (I32 ExtendU, Value.const_i32 16l, e)

  let load_32 m a = loadn m (concretize a) 4

  let load_64 _m _a = assert false

  let extract v h l =
    match v with
    | Val (Num (I32 i)) ->
      let i' = Int32.(logand 0xffl @@ shr_s i @@ of_int (l * 8)) in
      Value.const_i32 i'
    | Val (Num (I64 i)) ->
      let i' = Int64.(logand 0xffL @@ shr_s i @@ of_int (l * 8)) in
      Value.const_i32 (Int32.of_int64 i')
    | v' -> Extract (v', h, l)

  let storen m ~addr v n =
    let a0 = concretize addr in
    for i = 0 to n - 1 do
      let addr' = Int32.add a0 (Int32.of_int i)
      and v' = extract v (i + 1) i in
      Hashtbl.replace m.data addr' v'
    done

  let store_8 m ~addr v = storen m ~addr v 1

  let store_16 m ~addr v = storen m ~addr v 2

  let store_32 m ~addr v = storen m ~addr v 4

  let store_64 m ~addr v = storen m ~addr v 8

  let get_limit_max { limits; _ } =
    Option.map (fun i -> Value.const_i64 @@ Int64.of_int i) limits.max
end

module M' : Intf.Memory_data = M

let memory = M.create @@ Int32.of_int 2
