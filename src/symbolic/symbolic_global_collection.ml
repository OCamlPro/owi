(** Collections of globals *)

type global =
  { value : Symbolic_value.t
  ; env_id : int
  ; id : int
  }

let convert_values (v : Concrete_value.t) : Symbolic_value.t =
  match v with
  | I32 v -> I32 (Symbolic_i32.of_concrete v)
  | I64 v -> I64 (Symbolic_i64.of_concrete v)
  | F32 v -> F32 (Symbolic_f32.of_concrete v)
  | F64 v -> F64 (Symbolic_f64.of_concrete v)
  | V128 v -> V128 (Symbolic_v128.of_concrete v)
  | Ref (Func f) -> Ref (Func f)
  | Ref _ -> assert false

let global_of_concrete ~env_id ~id (v : Concrete_global.t) : global =
  let value = convert_values v.Concrete_global.value in
  { value; env_id; id }

module M = struct
  type symbolic = global
end

include Collection.Make (M)
