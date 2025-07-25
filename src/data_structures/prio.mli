type t =
  | Val of
      { instr_counter : int
      ; distances_to_unreachable : int list option
      }
  | Default
  | Random

val random : t

val default : t

val from_annotated : int Atomic.t -> int array option -> t

val compare : t -> t -> int
