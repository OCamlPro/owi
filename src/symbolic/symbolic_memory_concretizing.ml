module Backend = struct
  type address = Int32.t

  module Map = Map.Make (struct
    include Int32

    (* TODO: define this in Int32 directly? *)
    let compare i1 i2 = compare (Int32.to_int i1) (Int32.to_int i2)
  end)

  type t =
    { mutable data : Symbolic_value.int32 Map.t
    ; mutable chunks : Symbolic_value.int32 Map.t
    }

  let make () = { data = Map.empty; chunks = Map.empty }

  let clone { data; chunks } =
    (* Caution, it is tempting not to rebuild the record here, but...
       it must be! otherwise the mutable data points to the same location *)
    { data; chunks }

  let address a =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view a with
    | Val (Num (I32 i)) -> return i
    | Ptr { base; offset } ->
      select_i32 Symbolic_value.(I32.add (const_i32 base) offset)
    | _ -> select_i32 a

  let address_i32 a = a

  let load_byte a { data; _ } =
    match Map.find_opt a data with
    | None -> Smtml.Expr.value (Num (I8 0))
    | Some v -> v

  (* TODO: don't rebuild so many values it generates unecessary hc lookups *)
  let merge_extracts (e1, high, m1) (e2, m2, low) =
    let ty = Smtml.Expr.ty e1 in
    if m1 = m2 && Smtml.Expr.equal e1 e2 then
      if high - low = Smtml.Ty.size ty then e1
      else Smtml.Expr.extract e1 ~high ~low
    else
      Smtml.Expr.concat
        (Smtml.Expr.extract e1 ~high ~low:m1)
        (Smtml.Expr.extract e2 ~high:m2 ~low)

  let concat ~msb ~lsb offset =
    assert (offset > 0 && offset <= 8);
    match (Smtml.Expr.view msb, Smtml.Expr.view lsb) with
    | Val (Num (I8 i1)), Val (Num (I8 i2)) ->
      Symbolic_value.const_i32 Int32.(logor (shl (of_int i1) 8l) (of_int i2))
    | Val (Num (I8 i1)), Val (Num (I32 i2)) ->
      let offset = offset * 8 in
      if offset < 32 then
        Symbolic_value.const_i32
          Int32.(logor (shl (of_int i1) (of_int offset)) i2)
      else
        let i1' = Int64.of_int i1 in
        let i2' = Int64.of_int32 i2 in
        Symbolic_value.const_i64 Int64.(logor (shl i1' (of_int offset)) i2')
    | Val (Num (I8 i1)), Val (Num (I64 i2)) ->
      let offset = Int64.of_int (offset * 8) in
      Symbolic_value.const_i64 Int64.(logor (shl (of_int i1) offset) i2)
    | Extract (e1, h, m1), Extract (e2, m2, l) ->
      merge_extracts (e1, h, m1) (e2, m2, l)
    | Extract (e1, h, m1), Concat ({ node = Extract (e2, m2, l); _ }, e3) ->
      Smtml.Expr.concat (merge_extracts (e1, h, m1) (e2, m2, l)) e3
    | _ -> Smtml.Expr.concat msb lsb

  let loadn m a n =
    let rec loop addr size i acc =
      if i = size then acc
      else
        let addr' = Int32.(add addr (of_int i)) in
        let byte = load_byte addr' m in
        loop addr size (i + 1) (concat i ~msb:byte ~lsb:acc)
    in
    let v0 = load_byte a m in
    loop a n 1 v0

  let extract v pos =
    match Smtml.Expr.view v with
    | Val (Num (I8 _)) -> v
    | Val (Num (I32 i)) ->
      let i' = Int32.(to_int @@ logand 0xffl @@ shr_s i @@ of_int (pos * 8)) in
      Smtml.Expr.value (Num (I8 i'))
    | Val (Num (I64 i)) ->
      let i' = Int64.(to_int @@ logand 0xffL @@ shr_s i @@ of_int (pos * 8)) in
      Smtml.Expr.value (Num (I8 i'))
    | Cvtop
        (_, Zero_extend 24, ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym))
    | Cvtop
        (_, Sign_extend 24, ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym))
      ->
      sym
    | _ -> Smtml.Expr.extract v ~high:(pos + 1) ~low:pos

  let replace m k v = m.data <- Map.add k v m.data

  let storen m a v n =
    for i = 0 to n - 1 do
      let a' = Int32.add a (Int32.of_int i) in
      let v' = extract v i in
      replace m a' v'
    done

  let validate_address m a range =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view a with
    | Val (Num (I32 _)) ->
      (* An i32 is not a pointer to a heap chunk, so its valid *)
      return (Ok a)
    | Ptr { base; offset = start_offset } -> (
      let open Symbolic_value in
      match Map.find_opt base m.chunks with
      | None -> return (Error `Memory_leak_use_after_free)
      | Some chunk_size ->
        let+ is_out_of_bounds =
          let range = const_i32 (Int32.of_int (range - 1)) in
          (* end_offset: last byte we will read/write *)
          let end_offset = I32.add start_offset range in
          select
            (Bool.or_
               (I32.ge_u start_offset chunk_size)
               (I32.ge_u end_offset chunk_size) )
        in
        if is_out_of_bounds then Error `Memory_heap_buffer_overflow else Ok a )
    | _ ->
      (* A symbolic expression is valid, but we print to check if Ptr's are passing through here  *)
      Logs.warn (fun m -> m "Saw a symbolic address: %a" Smtml.Expr.pp a);
      return (Ok a)

  let ptr v =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view v with
    | Ptr { base; _ } -> return base
    | _ ->
      Logs.err (fun m ->
        m {|free: cannot fetch pointer base of "%a"|} Smtml.Expr.pp v );
      assert false

  let free m p =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view p with
    | Val (Num (I32 0l)) -> return (Symbolic_value.const_i32 0l)
    | _ ->
      let* base = ptr p in
      if not @@ Map.mem base m.chunks then trap `Double_free
      else begin
        let chunks = Map.remove base m.chunks in
        m.chunks <- chunks;
        return (Symbolic_value.const_i32 base)
      end

  let realloc m ~ptr ~size =
    let open Symbolic_choice_without_memory in
    let+ base = address ptr in
    let chunks = Map.add base size m.chunks in
    m.chunks <- chunks;
    Smtml.Expr.ptr base (Symbolic_value.const_i32 0l)
end

include Symbolic_memory_make.Make (Backend)
