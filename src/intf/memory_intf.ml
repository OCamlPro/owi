module type T = sig
  type t

  type i32

  type i64

  type 'a choice

  val load_8_s : t -> i32 -> i32 choice

  val load_8_u : t -> i32 -> i32 choice

  val load_16_s : t -> i32 -> i32 choice

  val load_16_u : t -> i32 -> i32 choice

  val load_32 : t -> i32 -> i32 choice

  val load_64 : t -> i32 -> i64 choice

  val store_8 : t -> addr:i32 -> i32 -> unit choice

  val store_16 : t -> addr:i32 -> i32 -> unit choice

  val store_32 : t -> addr:i32 -> i32 -> unit choice

  val store_64 : t -> addr:i32 -> i64 -> unit choice

  val grow : t -> i32 -> unit choice

  val fill : t -> pos:i32 -> len:i32 -> char -> unit choice

  val blit :
    src:t -> src_idx:i32 -> dst:t -> dst_idx:i32 -> len:i32 -> unit choice

  val blit_string : t -> string -> src:i32 -> dst:i32 -> len:i32 -> unit choice

  val size : t -> i32

  val size_in_pages : t -> i32

  val get_limit_max : t -> i64 option
end
