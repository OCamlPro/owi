open Smtml.Expr
module Expr = Smtml.Expr
module Value = Symbolic_value

module Make (_ : Choice_intf.Base) : Symbolic_memory_intf.M = struct
  type address = Int32.t

  type t =
    { data : (address, Value.int32) Hashtbl.t
    ; parent : t option
    ; chunks : (address, Value.int32) Hashtbl.t
    }

  let create () =
    { data = Hashtbl.create 128; parent = None; chunks = Hashtbl.create 16 }

  let clone m =
    { data = Hashtbl.create 16
    ; parent = Some m
    ; chunks = Hashtbl.copy m.chunks (* TODO: we can make this lazy as well *)
    }

  let address v =
    match view v with
    | Val (Num (I32 i)) -> i
    | _ -> Log.err {|Unsupported symbolic value reasoning over "%a"|} Expr.pp v

  let ptr v =
    match view v with
    | Ptr { base; _ } -> base
    | _ -> Log.err {|free: cannot fetch pointer base of "%a"|} Expr.pp v

  let address_i32 i = i

  let rec load_byte { parent; data; _ } a =
    try Hashtbl.find data a
    with Not_found -> (
      match parent with
      | None -> make (Val (Num (I8 0)))
      | Some parent -> load_byte parent a )

  (* TODO: don't rebuild so many values it generates unecessary hc lookups *)
  let merge_extracts (e1, h, m1) (e2, m2, l) =
    let ty = Expr.ty e1 in
    if m1 = m2 && Expr.equal e1 e2 then
      if h - l = Smtml.Ty.size ty then e1 else make (Extract (e1, h, l))
    else make (Concat (make (Extract (e1, h, m1)), make (Extract (e2, m2, l))))

  let concat ~msb ~lsb offset =
    assert (offset > 0 && offset <= 8);
    match (view msb, view lsb) with
    | Val (Num (I8 i1)), Val (Num (I8 i2)) ->
      Value.const_i32 Int32.(logor (shl (of_int i1) 8l) (of_int i2))
    | Val (Num (I8 i1)), Val (Num (I32 i2)) ->
      let offset = offset * 8 in
      if offset < 32 then
        Value.const_i32 Int32.(logor (shl (of_int i1) (of_int offset)) i2)
      else
        let i1' = Int64.of_int i1 in
        let i2' = Int64.of_int32 i2 in
        Value.const_i64 Int64.(logor (shl i1' (of_int offset)) i2')
    | Val (Num (I8 i1)), Val (Num (I64 i2)) ->
      let offset = Int64.of_int (offset * 8) in
      Value.const_i64 Int64.(logor (shl (of_int i1) offset) i2)
    | Extract (e1, h, m1), Extract (e2, m2, l) ->
      merge_extracts (e1, h, m1) (e2, m2, l)
    | Extract (e1, h, m1), Concat ({ node = Extract (e2, m2, l); _ }, e3) ->
      make (Concat (merge_extracts (e1, h, m1) (e2, m2, l), e3))
    | _ -> make (Concat (msb, lsb))

  let loadn m a n =
    let rec loop addr size i acc =
      if i = size then acc
      else
        let addr' = Int32.(add addr (of_int i)) in
        let byte = load_byte m addr' in
        loop addr size (i + 1) (concat i ~msb:byte ~lsb:acc)
    in
    let v0 = load_byte m a in
    loop a n 1 v0

  let extract v pos =
    match view v with
    | Val (Num (I8 _)) -> v
    | Val (Num (I32 i)) ->
      let i' = Int32.(to_int @@ logand 0xffl @@ shr_s i @@ of_int (pos * 8)) in
      value (Num (I8 i'))
    | Val (Num (I64 i)) ->
      let i' = Int64.(to_int @@ logand 0xffL @@ shr_s i @@ of_int (pos * 8)) in
      value (Num (I8 i'))
    | Cvtop (_, Zero_extend 24, ({ node = Symbol _; _ } as sym))
    | Cvtop (_, Sign_extend 24, ({ node = Symbol _; _ } as sym))
      when Smtml.Ty.equal (Ty_bitv 8) (ty sym) ->
      sym
    | _ -> make (Extract (v, pos + 1, pos))

  let storen m addr v n =
    for i = 0 to n - 1 do
      let addr' = Int32.add addr (Int32.of_int i) in
      let v' = extract v i in
      Hashtbl.replace m.data addr' v'
    done

  let is_within_bounds m a =
    match view a with
    | Val (Num (I32 a)) -> Ok (Value.Bool.const false, a)
    | Ptr { base; offset } -> (
      match Hashtbl.find m.chunks base with
      | exception Not_found -> Error Trap.Memory_leak_use_after_free
      | size ->
        let ptr = Int32.add base (address offset) in
        let upper_bound =
          Value.(I32.ge (const_i32 ptr) (I32.add (const_i32 base) size))
        in
        Ok (Value.Bool.(or_ (const (Int32.lt ptr base)) upper_bound), ptr) )
    | _ -> Log.err {|Unable to calculate address of: "%a"|} Expr.pp a

  let free m ptr =
    if not @@ Hashtbl.mem m.chunks ptr then
      Error Trap.Memory_leak_use_after_free
      (* failwith "Memory leak double free"; *)
    else Ok (Hashtbl.remove m.chunks ptr)

  let realloc m ptr size =
    Hashtbl.replace m.chunks ptr size;
    Expr.ptr ptr (Symbolic_value.const_i32 0l)
end
