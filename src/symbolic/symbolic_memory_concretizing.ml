module Backend = struct
  type address = Int32.t

  type t =
    { data : (address, Symbolic_value.int32) Hashtbl.t option ref
    ; parent : t option
    ; chunks : (address, Symbolic_value.int32) Hashtbl.t
    }

  let make () = { data = ref None; parent = None; chunks = Hashtbl.create 16 }

  let clone m =
    let parent = if Option.is_none !(m.data) then m.parent else Some m in
    (* TODO: Make chunk copying lazy *)
    { data = ref None; parent; chunks = Hashtbl.copy m.chunks }

  let address a =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view a with
    | Val (Num (I32 i)) -> return i
    | Ptr { base; offset } ->
      select_i32 Symbolic_value.(I32.add (const_i32 base) offset)
    | _ -> select_i32 a

  let address_i32 a = a

  let rec load_byte { parent; data; _ } a =
    let v =
      match !data with None -> None | Some data -> Hashtbl.find_opt data a
    in
    match v with
    | Some v -> v
    | None -> (
      match parent with
      | None -> Smtml.Expr.value (Num (I8 0))
      | Some parent -> load_byte parent a )

  (* TODO: don't rebuild so many values it generates unecessary hc lookups *)
  let merge_extracts (e1, h, m1) (e2, m2, l) =
    let ty = Smtml.Expr.ty e1 in
    if m1 = m2 && Smtml.Expr.equal e1 e2 then
      if h - l = Smtml.Ty.size ty then e1
      else Smtml.Expr.make (Extract (e1, h, l))
    else
      Smtml.Expr.(
        make (Concat (make (Extract (e1, h, m1)), make (Extract (e2, m2, l)))) )

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
      Smtml.Expr.(make (Concat (merge_extracts (e1, h, m1) (e2, m2, l), e3)))
    | _ -> Smtml.Expr.make (Concat (msb, lsb))

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
    | _ -> Smtml.Expr.make (Extract (v, pos + 1, pos))

  let replace m k v =
    match !(m.data) with
    | None ->
      let tbl = Hashtbl.create 16 in
      Hashtbl.add tbl k v;
      m.data := Some tbl
    | Some tbl -> Hashtbl.replace tbl k v

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
      match Hashtbl.find_opt m.chunks base with
      | None -> return (Error Trap.Memory_leak_use_after_free)
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
        if is_out_of_bounds then Error Trap.Memory_heap_buffer_overflow
        else Ok a )
    | _ ->
      (* A symbolic expression is valid, but we print to check if Ptr's are passing through here  *)
      Log.debug2 "Saw a symbolic address: %a@." Smtml.Expr.pp a;
      return (Ok a)

  let ptr v =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view v with
    | Ptr { base; _ } -> return base
    | _ ->
      Log.debug2 {|free: cannot fetch pointer base of "%a"|} Smtml.Expr.pp v;
      let* () = add_pc @@ Smtml.Expr.value False in
      assert false

  let free m p =
    let open Symbolic_choice_without_memory in
    let+ base = ptr p in
    if not @@ Hashtbl.mem m.chunks base then
      Fmt.failwith "Memory leak double free";
    Hashtbl.remove m.chunks base;
    Symbolic_value.const_i32 base

  let realloc m ~ptr ~size =
    let open Symbolic_choice_without_memory in
    let+ base = address ptr in
    Hashtbl.replace m.chunks base size;
    Smtml.Expr.ptr base (Symbolic_value.const_i32 0l)
end

include Symbolic_memory_make.Make (Backend)
