module type T = sig
  type t

  module Value : Value_intf.T

  module Choice : Choice_intf.Base with module Value := Value

  val load_8_s : t -> Value.i32 -> Value.i32 Choice.t

  val load_8_u : t -> Value.i32 -> Value.i32 Choice.t

  val load_16_s : t -> Value.i32 -> Value.i32 Choice.t

  val load_16_u : t -> Value.i32 -> Value.i32 Choice.t

  val load_32 : t -> Value.i32 -> Value.i32 Choice.t

  val load_64 : t -> Value.i32 -> Value.i64 Choice.t

  val store_8 : t -> addr:Value.i32 -> Value.i32 -> unit Choice.t

  val store_16 : t -> addr:Value.i32 -> Value.i32 -> unit Choice.t

  val store_32 : t -> addr:Value.i32 -> Value.i32 -> unit Choice.t

  val store_64 : t -> addr:Value.i32 -> Value.i64 -> unit Choice.t

  val grow : t -> Value.i32 -> unit

  val fill : t -> pos:Value.i32 -> len:Value.i32 -> char -> unit Choice.t

  val blit :
    t -> src:Value.i32 -> t -> dst:Value.i32 -> len:Value.i32 -> unit Choice.t

  val blit_string :
    t -> string -> src:Value.i32 -> dst:Value.i32 -> len:Value.i32 -> unit

  val size : t -> Value.i32

  val size_in_pages : t -> Value.i32

  val get_limit_max : t -> Value.i64 option
end
