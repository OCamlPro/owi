(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module ITbl = Hashtbl.Make (Int)

type t =
  { mutable value : Symbolic_value.t
  ; orig : Concrete_global.t
  }

type collection = t ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let global_copy r = { r with value = r.value }

let clone collection =
  (* TODO: this is ugly and should be rewritten... *)
  let s = Env_id.Tbl.to_seq collection in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, global_copy a)) s) )
       s

let convert_values (v : Concrete_value.t) : Symbolic_value.t =
  (* TODO share various versions *)
  match v with
  | I32 v -> I32 (Symbolic_value.const_i32 v)
  | I64 v -> I64 (Symbolic_value.const_i64 v)
  | F32 v -> F32 (Symbolic_value.const_f32 v)
  | F64 v -> F64 (Symbolic_value.const_f64 v)
  | V128 v -> V128 (Symbolic_value.const_v128 v)
  | Ref (Funcref f) -> Ref (Funcref f)
  | Ref _ -> assert false

let convert (orig_global : Concrete_global.t) : t =
  { value = convert_values orig_global.value; orig = orig_global }

let get_env env_id tables =
  match Env_id.Tbl.find_opt tables env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add tables env_id t;
    t

let get_global env_id (orig_global : Concrete_global.t) collection g_id =
  let env = get_env env_id collection in
  match ITbl.find_opt env g_id with
  | Some t -> t
  | None ->
    let t = convert orig_global in
    ITbl.add env g_id t;
    t

let value v = v.value

let set_value v x = v.value <- x

let mut v = v.orig.mut

let typ v = v.orig.typ
