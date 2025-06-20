type t = Concrete_memory.t * Symbolic_memory.t

module C = Concrete_memory
module S = Symbolic_memory

let with_concrete (mc, ms) a f_c f_s =
  let open Concolic_choice in
  let* a = select_i32 a in
  match f_c mc a with
  | Error t -> Concolic_choice.trap t
  | Ok c -> Concolic_choice.return (c, f_s ms (Symbolic_value.const_i32 a))

let with_concrete_store (mc, ms) a f_c f_s (vc, vs) =
  let open Concolic_choice in
  let+ addr = select_i32 a in
  f_c mc ~addr vc;
  f_s ms ~addr:(Symbolic_value.const_i32 addr) vs

let load_8_s m a : Concolic_value.int32 Concolic_choice.t =
  with_concrete m a C.load_8_s S.load_8_s

let load_8_u m a = with_concrete m a C.load_8_u S.load_8_u

let load_16_s m a = with_concrete m a C.load_16_s S.load_16_s

let load_16_u m a = with_concrete m a C.load_16_u S.load_16_u

let load_32 m a = with_concrete m a C.load_32 S.load_32

let load_64 m a = with_concrete m a C.load_64 S.load_64

let store_8 m ~addr v = with_concrete_store m addr C.store_8 S.store_8 v

let store_16 m ~addr v = with_concrete_store m addr C.store_16 S.store_16 v

let store_32 m ~addr v = with_concrete_store m addr C.store_32 S.store_32 v

let store_64 m ~addr v = with_concrete_store m addr C.store_64 S.store_64 v

let grow (mc, ms) (deltac, deltas) =
  Concrete_memory.grow mc deltac;
  Symbolic_memory.grow ms deltas

let size (mc, ms) = (Concrete_memory.size mc, Symbolic_memory.size ms)

let size_in_pages (mc, ms) =
  (Concrete_memory.size_in_pages mc, Symbolic_memory.size_in_pages ms)

let fill _ = Fmt.failwith "TODO"

let blit _ = Fmt.failwith "TODO"

let blit_string (mc, ms) str ~src ~dst ~len =
  let src_c, src_s = src in
  let dst_c, dst_s = dst in
  let len_c, len_s = len in
  Concrete_memory.blit_string mc str ~src:src_c ~dst:dst_c ~len:len_c;
  Symbolic_memory.blit_string ms str ~src:src_s ~dst:dst_s ~len:len_s;
  ()

let get_limit_max _ = Fmt.failwith "TODO"
