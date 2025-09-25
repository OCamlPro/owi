type t =
  | Val of
      { instr_counter : int
      ; distances_to_unreachable : int list option
      }
  | Default
  | Random of int

val random : t

val default : t

val from_annotated : int -> int array option -> t

val compare : t -> t -> int
