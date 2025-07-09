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
    | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
      return (Smtml.Bitvector.to_int32 bv)
    | Ptr { base; offset } ->
      let base = Smtml.Bitvector.to_int32 base |> Symbolic_value.const_i32 in
      let addr = Symbolic_value.I32.add base offset in
      select_i32 addr
    | _ -> select_i32 a

  let address_i32 a = a

  let load_byte a { data; _ } =
    match Map.find_opt a data with
    | None -> Smtml.Expr.value (Bitv (Smtml.Bitvector.of_int8 0))
    | Some v -> v

  let loadn m a n =
    let rec loop addr size i acc =
      if i = size then acc
      else
        let addr' = Int32.(add addr (of_int i)) in
        let byte = load_byte addr' m in
        loop addr size (i + 1) (Smtml.Expr.concat byte acc)
    in
    let v0 = load_byte a m in
    loop a n 1 v0

  let replace m k v = m.data <- Map.add k v m.data

  let storen m a v n =
    for i = 0 to n - 1 do
      let a' = Int32.add a (Int32.of_int i) in
      let v' = Smtml.Expr.extract v ~low:i ~high:(i + 1) in
      replace m a' v'
    done

  let validate_address m a range =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view a with
    | Val (Bitv _) ->
      (* An i32 is not a pointer to a heap chunk, so its valid *)
      return (Ok a)
    | Ptr { base; offset = start_offset } -> (
      let open Symbolic_value in
      let base = Smtml.Bitvector.to_int32 base in
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
            ~prio_true:Prio.Default ~prio_false:Prio.Default
        in
        if is_out_of_bounds then Error `Memory_heap_buffer_overflow else Ok a )
    | _ ->
      (* A symbolic expression is valid, but we print to check if Ptr's are passing through here  *)
      Logs.warn (fun m -> m "Saw a symbolic address: %a" Smtml.Expr.pp a);
      return (Ok a)

  let ptr v =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view v with
    | Ptr { base; _ } ->
      let base = Smtml.Bitvector.to_int32 base in
      return base
    | _ ->
      Logs.err (fun m ->
        m {|free: cannot fetch pointer base of "%a"|} Smtml.Expr.pp v );
      assert false

  let free m p =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view p with
    | Val (Bitv bv) when Smtml.Bitvector.eqz bv ->
      return (Symbolic_value.const_i32 0l)
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
