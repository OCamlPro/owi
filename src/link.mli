val set_table :
  Simplify.module_ array ->
  int -> int -> (int * Types.const) option array -> unit
val get_table :
  Simplify.module_ array ->
  int ->
  int ->
  (int * int) * Types.ref_type * (int * Types.const) option array *
  int option

  val set_global :
  Simplify.module_ array -> int -> int -> Types.const -> unit
val get_global :
  Simplify.module_ array ->
  int -> int -> int * Types.global_type * Types.const

  val get_func : Simplify.module_ array -> int -> int -> int * Types.func

val set_memory : Simplify.module_ array -> int -> int -> bytes -> unit
val get_memory : Simplify.module_ array -> int -> int -> bytes * int option

val module_ : 'a -> Simplify.module_ array -> int -> unit
