(** Collections of memories *)

module Map = Map.Make (Int32)

type memory =
  { mutable data : Symbolic_i32.t Map.t
  ; mutable chunks : Symbolic_i32.t Map.t
  ; mutable size : Symbolic_i32.t
  }

let create_one size =
  { data = Map.empty; chunks = Map.empty; size = Symbolic_i32.of_concrete size }

let memory_of_concrete (original : Concrete_memory.t) : memory =
  let s = Concrete_memory.size_in_pages original in
  (* TODO: how come we don't put anything in here? is it always an uninitialized memory ? *)
  create_one s

module M = struct
  type symbolic = memory

  (* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
  let clone_one { data; chunks; size } = { data; chunks; size }
end

include Collection.Make (M)
