(** Collections of memories *)

module Map = Map.Make (Int32)

type memory =
  { mutable data : Symbolic_i32.t Map.t
  ; mutable chunks : Symbolic_i32.t Map.t
  ; mutable size : Symbolic_i32.t
  }

let create_one size =
  { data = Map.empty; chunks = Map.empty; size = Symbolic_i32.of_concrete size }

module M = struct
  type concrete = Concrete_memory.t

  type symbolic = memory

  let convert_one original =
    let s = Concrete_memory.size_in_pages original in
    create_one s

  (* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
  let clone_one { data; chunks; size } = { data; chunks; size }
end

include Collection.Make (M)
