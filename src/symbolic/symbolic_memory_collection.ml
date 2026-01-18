(** Collections of memories *)

module Map = Map.Make (Int32)

type memory =
  { mutable data : Symbolic_i32.t Map.t
  ; mutable chunks : Symbolic_i32.t Map.t
  ; mutable size : Symbolic_i32.t
  }

let create_one size =
  { data = Map.empty; chunks = Map.empty; size = Symbolic_i32.of_concrete size }

(* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
let clone_one { data; chunks; size } = { data; chunks; size }

let convert (orig_mem : Concrete_memory.t) : memory =
  let s = Concrete_memory.size_in_pages orig_mem in
  create_one s

type collection = (int * int, memory) Hashtbl.t

let init () = Hashtbl.create 16

let clone collection =
  let collection' = init () in
  Hashtbl.iter
    (fun loc memory ->
      let memory' = clone_one memory in
      Hashtbl.add collection' loc memory' )
    collection;
  collection'

let get_memory env_id (orig_memory : Concrete_memory.t) collection g_id =
  let loc = (env_id, g_id) in
  match Hashtbl.find_opt collection loc with
  | None ->
    let g = convert orig_memory in
    Hashtbl.add collection loc g;
    g
  | Some t -> t
