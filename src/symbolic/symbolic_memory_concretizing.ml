module Backend = struct
  module Map = Map.Make (Int32)

  type t =
    { mutable data : Symbolic_value.i32 Map.t
    ; mutable chunks : Symbolic_value.i32 Map.t
    }

  let make () = { data = Map.empty; chunks = Map.empty }

  (* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
  let clone { data; chunks } =
    (* WARNING: it is tempting not to rebuild the record here, but...
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
            (Boolean.or_
               (I32.ge_u start_offset chunk_size)
               (I32.ge_u end_offset chunk_size) )
            ~prio_true:Prio.Default ~prio_false:Prio.Default
        in
        if is_out_of_bounds then Error `Memory_heap_buffer_overflow else Ok a )
    | _ ->
      (* A symbolic expression is valid, but we print to check if Ptr's are passing through here  *)
      Log.warn (fun m -> m "Saw a symbolic address: %a" Smtml.Expr.pp a);
      return (Ok a)

  let ptr v =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view v with
    | Ptr { base; _ } ->
      let base = Smtml.Bitvector.to_int32 base in
      return base
    | _ ->
      Log.err (fun m ->
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

let page_size = Symbolic_value.const_i32 65_536l

type t =
  { data : Backend.t
  ; mutable size : Symbolic_value.i32
  }

let create size =
  { data = Backend.make (); size = Symbolic_value.const_i32 size }

let i32 v =
  match Smtml.Expr.view v with
  | Val (Bitv i) when Smtml.Bitvector.numbits i = 32 ->
    Smtml.Bitvector.to_int32 i
  | _ -> assert false

let grow m delta =
  let old_size = Symbolic_value.I32.mul m.size page_size in
  let new_size = Symbolic_value.I32.(div (add old_size delta) page_size) in
  m.size <-
    Symbolic_value.Boolean.select_expr
      (Symbolic_value.I32.gt new_size m.size)
      ~if_true:new_size ~if_false:m.size

let size { size; _ } = Symbolic_value.I32.mul size page_size

let size_in_pages { size; _ } = size

(* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
let clone_memory m = { data = Backend.clone m.data; size = m.size }

let must_be_valid_address m a n =
  let open Symbolic_choice_without_memory in
  let* addr = Backend.validate_address m a n in
  match addr with Error t -> trap t | Ok ptr -> Backend.address ptr

let load_8_s m a =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data a 1 in
  let v = Backend.loadn m.data a 1 in
  match Smtml.Expr.view v with
  | Val (Bitv i8) when Smtml.Bitvector.numbits i8 = 8 ->
    let i8 = Smtml.Bitvector.to_int32 i8 in
    Symbolic_value.const_i32 (Int32.extend_s 8 i8)
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Sign_extend 24) v

let load_8_u m a =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data a 1 in
  let v = Backend.loadn m.data a 1 in
  match Smtml.Expr.view v with
  | Val (Bitv i) when Smtml.Bitvector.numbits i = 8 ->
    let i = Smtml.Bitvector.to_int32 i in
    Symbolic_value.const_i32 i
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Zero_extend 24) v

let load_16_s m a =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data a 2 in
  let v = Backend.loadn m.data a 2 in
  match Smtml.Expr.view v with
  | Val (Bitv i16) when Smtml.Bitvector.numbits i16 = 16 ->
    let i16 = Smtml.Bitvector.to_int32 i16 in
    Symbolic_value.const_i32 (Int32.extend_s 16 i16)
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Sign_extend 16) v

let load_16_u m a =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data a 2 in
  let v = Backend.loadn m.data a 2 in
  match Smtml.Expr.view v with
  | Val (Bitv i16) when Smtml.Bitvector.numbits i16 = 16 ->
    let i16 = Smtml.Bitvector.to_int32 i16 in
    Symbolic_value.const_i32 i16
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Zero_extend 16) v

let load_32 m a =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data a 4 in
  let res = Backend.loadn m.data a 4 in
  Smtml.Expr.simplify res

let load_64 m a =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data a 8 in
  Backend.loadn m.data a 8

let store_8 m ~addr v =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data addr 1 in
  Backend.storen m.data a v 1

let store_16 m ~addr v =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data addr 2 in
  Backend.storen m.data a v 2

let store_32 m ~addr v =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data addr 4 in
  Backend.storen m.data a v 4

let store_64 m ~(addr : Smtml.Expr.t) v =
  let open Symbolic_choice_without_memory in
  let+ a = must_be_valid_address m.data addr 8 in
  Backend.storen m.data a v 8

let fill m ~(pos : Smtml.Expr.t) ~(len : Smtml.Expr.t) (c : char) =
  let open Symbolic_choice_without_memory in
  let* len = select_i32 len in
  let len = Int32.to_int len in
  let* pos = select_i32 pos in
  let pos = Int32.to_int pos in
  let c = Symbolic_value.const_i32 (Int32.of_int (int_of_char c)) in

  let rec aux i =
    if i = len then return ()
    else
      let addr = Symbolic_value.const_i32 (Int32.of_int (pos + i)) in
      let* () = store_8 m ~addr c in
      aux (i + 1)
  in
  aux 0

let blit m ~src ~dst ~len =
  let open Symbolic_choice_without_memory in
  let* len = select_i32 len in
  let len = Int32.to_int len in
  let* src = select_i32 src in
  let src = Int32.to_int src in
  let* dst = select_i32 dst in
  let dst = Int32.to_int dst in

  let rec aux i =
    if i = len then return ()
    else
      let addr = Symbolic_value.const_i32 (Int32.of_int (src + i)) in
      let* v = load_8_s m addr in
      let addr = Symbolic_value.const_i32 (Int32.of_int (dst + i)) in
      let* () = store_8 m ~addr v in
      aux (i + 1)
  in
  aux 0

let blit_string m str ~src ~dst ~len =
  (* This function is only used in memory init so everything will be concrete *)
  (* TODO: I am not sure this is true, this should be investigated and fixed at some point *)
  let src = Int32.to_int @@ i32 src in
  let dst = Int32.to_int @@ i32 dst in
  let len = Int32.to_int @@ i32 len in
  for i = 0 to len - 1 do
    let byte = Char.code @@ String.get str (src + i) in
    let a = Backend.address_i32 (Int32.of_int (dst + i)) in
    Backend.storen m.data a
      (Smtml.Expr.value (Bitv (Smtml.Bitvector.of_int8 byte)))
      1
  done

let get_limit_max _m = None (* TODO *)

let free m base = Backend.free m.data base

let realloc m ~ptr ~size = Backend.realloc m.data ~ptr ~size

let convert (orig_mem : Concrete_memory.t) : t =
  let s = Concrete_memory.size_in_pages orig_mem in
  create s

(** Collections of memories *)

type collection = (int * int, t) Hashtbl.t

let init () = Hashtbl.create 16

let clone collection =
  let collection' = init () in
  Hashtbl.iter
    (fun loc memory ->
      let memory' = clone_memory memory in
      Hashtbl.add collection' loc memory' )
    collection;
  collection'

let get_memory env_id (orig_memory : Concrete_memory.t) collection g_id =
  let loc = (env_id, g_id) in
  match Hashtbl.find_opt collection loc with
  | None ->
    let g = convert orig_memory in
    Hashtbl.add collection loc g;
    g
  | Some t -> t
