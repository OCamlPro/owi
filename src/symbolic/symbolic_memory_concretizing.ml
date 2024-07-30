module Backend = struct
  open Smtml

  type address = Int32.t

  type t =
    { data : (address, Symbolic_value.int32) Hashtbl.t
    ; parent : t option
    ; chunks : (address, Symbolic_value.int32) Hashtbl.t
    }

  let make () =
    { data = Hashtbl.create 16; parent = None; chunks = Hashtbl.create 16 }

  let clone m =
    (* TODO: Make chunk copying lazy *)
    { data = Hashtbl.create 16
    ; parent = Some m
    ; chunks = Hashtbl.copy m.chunks
    }

  let address a =
    let open Symbolic_choice_without_memory in
    match Expr.view a with
    | Val (Num (I32 i)) -> return i
    | Ptr { base; offset } ->
      select_i32 Symbolic_value.(I32.add (const_i32 base) offset)
    | _ -> select_i32 a

  let address_i32 a = a

  let rec load_byte { parent; data; _ } a =
    try Hashtbl.find data a
    with Not_found -> (
      match parent with
      | None -> Expr.value (Num (I8 0))
      | Some parent -> load_byte parent a )

  (* TODO: don't rebuild so many values it generates unecessary hc lookups *)
  let merge_extracts (e1, h, m1) (e2, m2, l) =
    let ty = Expr.ty e1 in
    if m1 = m2 && Expr.equal e1 e2 then
      if h - l = Ty.size ty then e1 else Expr.make (Extract (e1, h, l))
    else
      Expr.(
        make (Concat (make (Extract (e1, h, m1)), make (Extract (e2, m2, l)))) )

  let concat ~msb ~lsb offset =
    assert (offset > 0 && offset <= 8);
    match (Expr.view msb, Expr.view lsb) with
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
      Expr.(make (Concat (merge_extracts (e1, h, m1) (e2, m2, l), e3)))
    | _ -> Expr.make (Concat (msb, lsb))

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
    match Expr.view v with
    | Val (Num (I8 _)) -> v
    | Val (Num (I32 i)) ->
      let i' = Int32.(to_int @@ logand 0xffl @@ shr_s i @@ of_int (pos * 8)) in
      Expr.value (Num (I8 i'))
    | Val (Num (I64 i)) ->
      let i' = Int64.(to_int @@ logand 0xffL @@ shr_s i @@ of_int (pos * 8)) in
      Expr.value (Num (I8 i'))
    | Cvtop
        (_, Zero_extend 24, ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym))
    | Cvtop
        (_, Sign_extend 24, ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym))
      ->
      sym
    | _ -> Expr.make (Extract (v, pos + 1, pos))

  let storen m a v n =
    for i = 0 to n - 1 do
      let a' = Int32.add a (Int32.of_int i) in
      let v' = extract v i in
      Hashtbl.replace m.data a' v'
    done

  let validate_address m a =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view a with
    | Val (Num (I32 _)) -> return (Ok a) (* An i32 is a valid address *)
    | Ptr { base; offset } -> (
      let open Symbolic_value in
      (* A pointer is valid if it's within bounds. *)
      match Hashtbl.find m.chunks base with
      | exception Not_found -> return (Error Trap.Memory_leak_use_after_free)
      | size ->
        let+ is_out_of_bounds = select (I32.ge_u offset size) in
        if is_out_of_bounds then Error Trap.Memory_heap_buffer_overflow
        else Ok a )
    | _ ->
      (* A symbolic expression should be a valid address *)
      return (Ok a)

  let ptr v =
    let open Symbolic_choice_without_memory in
    match Expr.view v with
    | Ptr { base; _ } -> return base
    | _ ->
      Log.debug2 {|free: cannot fetch pointer base of "%a"|} Expr.pp v;
      let* () = add_pc @@ Expr.value False in
      assert false

  let free m p =
    let open Symbolic_choice_without_memory in
    let+ base = ptr p in
    if not @@ Hashtbl.mem m.chunks base then
      Fmt.failwith "Memory leak double free";
    Hashtbl.remove m.chunks base

  let realloc m ~ptr ~size =
    let open Symbolic_choice_without_memory in
    let+ base = address ptr in
    Hashtbl.replace m.chunks base size;
    Expr.ptr base (Symbolic_value.const_i32 0l)

  let rec pp fmt m =
    let pp_parent =
      Fmt.option ~none:(fun fmt () -> Fmt.string fmt "None (root mem)") pp
    in
    let pp_v fmt (a, b) = Fmt.pf fmt "0x%08lx %a" a Expr.pp b in
    Fmt.pf fmt "@[<v>parent:@;@[<v 1> %a@]@;data:@;@[<v 1> %a@]@]" pp_parent
      m.parent (Fmt.hashtbl pp_v) m.data
end

include Symbolic_memory_make.Make (Backend)
