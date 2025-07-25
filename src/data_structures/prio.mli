type t =
  | Val of
      { instr_counter : int
      ; distances_to_unreachable : int list
      }
  | Default
  | Random

val random : t

val default : t

val from_annotated : int Atomic.t -> int array -> t

val compare : t -> t -> int
