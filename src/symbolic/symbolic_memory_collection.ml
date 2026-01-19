(** Collections of memories *)

module Map = Map.Make (Int32)

type memory =
  { data : Symbolic_i32.t Map.t
  ; chunks : Symbolic_i32.t Map.t
  ; size : Symbolic_i32.t
  ; env_id : int
  ; id : int
  }

let memory_of_concrete ~env_id ~id (original : Concrete_memory.t) : memory =
  let size = Concrete_memory.size_in_pages original in
  (* TODO: how come we don't put anything in here? is it always an uninitialized memory ? *)
  { data = Map.empty
  ; chunks = Map.empty
  ; size = Symbolic_i32.of_concrete size
  ; env_id
  ; id
  }

module M = struct
  type symbolic = memory
end

include Collection.Make (M)
