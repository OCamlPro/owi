module Map = Map.Make (Int32)

type int8 = Smtml.Typed.bitv8 Smtml.Typed.t

type t = Symbolic_memory0.t =
  { data : int8 Map.t
  ; chunks : Symbolic_i32.t Map.t
  ; size : Symbolic_i32.t
  ; env_id : int
  ; id : int
  }

let replace memory =
  Symbolic_choice.modify_thread (Thread.replace_memory memory)

let address a =
  let open Symbolic_choice in
  match Smtml.Typed.view a with
  | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
    return (Smtml.Bitvector.to_int32 bv)
  | Ptr { base; offset } ->
    let base =
      (* TODO: it seems possible to avoid this conversion *)
      Smtml.Bitvector.to_int32 base |> Symbolic_i32.of_concrete
    in
    let offset = Smtml.Typed.Unsafe.wrap offset in
    let addr = Symbolic_i32.add base offset in
    select_i32 addr
  | _ -> select_i32 a

let load_byte a { data; _ } =
  match Map.find_opt a data with
  | None -> Smtml.Typed.Bitv8.v (Smtml.Bitvector.of_int8 0)
  | Some v -> v

let replace_byte a (v : int8) data = Map.add a v data

let validate_address m a range =
  let open Symbolic_choice in
  match Smtml.Typed.view a with
  | Val (Bitv _) ->
    (* An i32 is not a pointer to a heap chunk, so its valid *)
    return (Ok a)
  | Ptr { base; offset = start_offset } -> (
    let base = Smtml.Bitvector.to_int32 base in
    match Map.find_opt base m.chunks with
    | None -> return (Error `Memory_leak_use_after_free)
    | Some chunk_size ->
      let+ is_out_of_bounds =
        let range = Symbolic_i32.of_int (range - 1) in
        (* end_offset: last byte we will read/write *)
        let start_offset = Smtml.Typed.Unsafe.wrap start_offset in
        let end_offset = Symbolic_i32.add start_offset range in
        (* TODO: better prio here *)
        let prio_true = Prio.dummy in
        let prio_false = Prio.dummy in
        select
          (Symbolic_boolean.or_
             (Symbolic_i32.ge_u start_offset chunk_size)
             (Symbolic_i32.ge_u end_offset chunk_size) )
          ~prio_true ~prio_false
      in
      if is_out_of_bounds then Error `Memory_heap_buffer_overflow else Ok a )
  | _ ->
    (* A symbolic expression is valid, but we print to check if Ptr's are passing through here  *)
    Log.warn (fun m -> m "Saw a symbolic address: %a" Smtml.Typed.Bitv32.pp a);
    return (Ok a)

let ptr v =
  let open Symbolic_choice in
  match Smtml.Typed.view v with
  | Ptr { base; _ } ->
    let base = Smtml.Bitvector.to_int32 base in
    return base
  | _ ->
    Log.err (fun m ->
      m {|free: cannot fetch pointer base of "%a"|} Smtml.Typed.Bitv32.pp v );
    assert false

let free m p : Symbolic_i32.t Symbolic_choice.t =
  let open Symbolic_choice in
  match Smtml.Typed.view p with
  | Val (Bitv bv) when Smtml.Bitvector.eqz bv ->
    return (Symbolic_i32.of_concrete 0l)
  | _ ->
    let* base = ptr p in
    if not @@ Map.mem base m.chunks then trap `Double_free
    else begin
      let chunks = Map.remove base m.chunks in
      let m = { m with chunks } in
      let* () = replace m in
      return (Symbolic_i32.of_concrete base)
    end

let realloc m ~(ptr : Symbolic_i32.t) ~(size : Symbolic_i32.t) =
  let open Symbolic_choice in
  let* base = address ptr in
  let chunks = Map.add base size m.chunks in
  let m = { m with chunks } in
  let+ () = replace m in
  Smtml.Typed.ptr base (Symbolic_i32.of_concrete 0l)

let page_size = Symbolic_i32.of_concrete 65_536l

(******************************************)

let i32 v =
  match Smtml.Expr.view v with
  | Val (Bitv i) when Smtml.Bitvector.numbits i = 32 ->
    Smtml.Bitvector.to_int32 i
  | _ -> assert false

let grow m delta =
  let old_size = Symbolic_i32.mul m.size page_size in
  let new_size = Symbolic_i32.(div (add old_size delta) page_size) in
  let size =
    Symbolic_boolean.ite
      (Symbolic_i32.gt new_size m.size)
      ~if_true:new_size ~if_false:m.size
  in
  let m = { m with size } in
  replace m

let size { size; _ } = Symbolic_i32.mul size page_size

let size_in_pages { size; _ } = size

let must_be_valid_address m a n =
  let open Symbolic_choice in
  let* addr = validate_address m a n in
  match addr with Error t -> trap t | Ok ptr -> address ptr

let load_8_s m a =
  let open Symbolic_choice in
  let+ a = must_be_valid_address m a 1 in
  let v = load_byte a m in
  Smtml.Typed.Bitv32.of_int8_s v

let load_8_u m a =
  let open Symbolic_choice in
  let+ a = must_be_valid_address m a 1 in
  let v = load_byte a m in
  Smtml.Typed.Bitv32.of_int8_u v

let load_16_unchecked m a : Smtml.Typed.bitv16 Smtml.Typed.t =
  let lsb = load_byte a m in
  let msb = load_byte (Int32.add a 1l) m in
  Smtml.Typed.Bitv8.concat msb lsb

let load_16_s m a =
  let open Symbolic_choice in
  let+ a = must_be_valid_address m a 2 in
  let v = load_16_unchecked m a in
  Smtml.Typed.Bitv32.of_int16_s v

let load_16_u m a =
  let open Symbolic_choice in
  let+ a = must_be_valid_address m a 2 in
  let v = load_16_unchecked m a in
  Smtml.Typed.Bitv32.of_int16_u v

let load_32_unchecked m a : Smtml.Typed.bitv32 Smtml.Typed.t =
  let low = load_16_unchecked m a in
  let high = load_16_unchecked m (Int32.add a 2l) in
  Smtml.Typed.Bitv16.concat high low

let load_32 m a =
  let open Symbolic_choice in
  let+ a = must_be_valid_address m a 4 in
  let v = load_32_unchecked m a in
  Smtml.Typed.simplify v

let load_64 m a =
  let open Symbolic_choice in
  let+ a = must_be_valid_address m a 8 in
  let low = load_32_unchecked m a in
  let high = load_32_unchecked m (Int32.add a 4l) in
  Smtml.Typed.Bitv32.concat high low

let store_8 m ~addr v =
  let open Symbolic_choice in
  let* a = must_be_valid_address m addr 1 in
  let data =
    replace_byte a (Smtml.Typed.Bitv32.extract v ~high:1 ~low:0) m.data
  in
  replace { m with data }

let store_16 m ~addr v =
  let open Symbolic_choice in
  let* a = must_be_valid_address m addr 2 in
  let data =
    replace_byte a (Smtml.Typed.Bitv32.extract v ~high:1 ~low:0) m.data
    |> replace_byte (Int32.add a 1l)
         (Smtml.Typed.Bitv32.extract v ~high:2 ~low:1)
  in
  replace { m with data }

let store_byte_list data start_addr bytes =
  let rec loop data offset = function
    | [] -> data
    | byte :: remaining ->
      let addr = Int32.add start_addr offset in
      let data = replace_byte addr byte data in
      loop data (Int32.add offset 1l) remaining
  in
  loop data 0l bytes

let store_32 m ~addr v =
  let open Symbolic_choice in
  let* a = must_be_valid_address m addr 4 in
  let data = store_byte_list m.data a (Smtml.Typed.Bitv32.to_bytes v) in
  replace { m with data }

let store_64 m ~(addr : Symbolic_i32.t) v =
  let open Symbolic_choice in
  let* a = must_be_valid_address m addr 8 in
  let data = store_byte_list m.data a (Smtml.Typed.Bitv64.to_bytes v) in
  replace { m with data }

(* This function uses `m` for bounds checks but return an updated version of `data` *)
let store_8_no_replace m data ~(addr : Symbolic_i32.t) v =
  let open Symbolic_choice in
  let+ a = must_be_valid_address m addr 1 in
  replace_byte a (Smtml.Typed.Bitv32.extract v ~high:1 ~low:0) data

let fill m ~(pos : Symbolic_i32.t) ~(len : Symbolic_i32.t) (c : char) =
  let open Symbolic_choice in
  let* len = select_i32 len in
  let len = Int32.to_int len in
  let* pos = select_i32 pos in
  let pos = Int32.to_int pos in
  let c = Symbolic_i32.of_int (int_of_char c) in

  let rec loop i data =
    if i = len then return data
    else
      let addr = Symbolic_i32.of_int (pos + i) in
      let* data = store_8_no_replace m data ~addr c in
      loop (i + 1) data
  in
  let* data = loop 0 m.data in
  replace { m with data }

let blit ~src ~src_idx ~dst ~dst_idx ~len =
  let open Symbolic_choice in
  let* len = select_i32 len in
  let len = Int32.to_int len in
  let* src_idx = select_i32 src_idx in
  let src_idx = Int32.to_int src_idx in
  let* dst_idx = select_i32 dst_idx in
  let dst_idx = Int32.to_int dst_idx in

  let rec loop i data =
    if i = len then return data
    else
      let addr = Symbolic_i32.of_int (src_idx + i) in
      let* v = load_8_s src addr in
      let addr = Symbolic_i32.of_int (dst_idx + i) in
      let* data = store_8_no_replace dst data ~addr v in
      loop (i + 1) data
  in
  let* data = loop 0 dst.data in
  replace { dst with data }

let blit_string m str ~src ~dst ~len =
  (* This function is only used in memory init so everything will be concrete *)
  (* TODO: I am not sure this is true, this should be investigated and fixed at some point *)
  let open Symbolic_choice in
  let src = Smtml.Typed.Unsafe.unwrap src in
  let dst = Smtml.Typed.Unsafe.unwrap dst in
  let len = Smtml.Typed.Unsafe.unwrap len in
  let src = Int32.to_int @@ i32 src in
  let dst = Int32.to_int @@ i32 dst in
  let len = Int32.to_int @@ i32 len in
  let rec loop i data =
    if i = len then return data
    else
      let byte = Char.code @@ String.get str (src + i) in
      let addr = Symbolic_i32.of_int (dst + i) in
      let* data =
        store_8_no_replace m data ~addr
          (Smtml.Typed.Bitv32.v (Smtml.Bitvector.of_int8 byte))
      in
      loop (i + 1) data
  in
  let* data = loop 0 m.data in
  replace { m with data }

let get_limit_max _m = None (* TODO *)

let of_concrete ~env_id ~id (original : Concrete_memory.t) : t =
  let size = Concrete_memory.size_in_pages original in
  (* TODO: how come we don't put anything in here? is it always an uninitialized memory ? *)
  { data = Map.empty
  ; chunks = Map.empty
  ; size = Symbolic_i32.of_concrete size
  ; env_id
  ; id
  }
