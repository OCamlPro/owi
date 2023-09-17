(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type global =
  { mutable value : Symbolic_value.S.t
  ; orig : Concrete_global.t
  }

type globals = global ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let global_copy r = { r with value = r.value }

let clone (globals : globals) : globals =
  let s = Env_id.Tbl.to_seq globals in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, global_copy a)) s) )
       s

let convert_values (v : Concrete_value.t) : Symbolic_value.S.t =
  (* TODO share various versions *)
  match v with
  | I32 v -> I32 (Symbolic_value.S.const_i32 v)
  | I64 v -> I64 (Symbolic_value.S.const_i64 v)
  | F32 v -> F32 (Symbolic_value.S.const_f32 v)
  | F64 v -> F64 (Symbolic_value.S.const_f64 v)
  | Ref (Funcref f) -> Ref (Funcref f)
  | Ref _ -> assert false

let convert (orig_global : Concrete_global.t) : global =
  { value = convert_values orig_global.value; orig = orig_global }

let get_env env_id tables =
  match Env_id.Tbl.find_opt tables env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add tables env_id t;
    t

let get_global env_id (orig_global : Concrete_global.t) (tables : globals) g_id
    =
  let env = get_env env_id tables in
  match ITbl.find_opt env g_id with
  | Some t -> t
  | None ->
    let t = convert orig_global in
    ITbl.add env g_id t;
    t
