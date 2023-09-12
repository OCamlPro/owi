(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type table = Sym_value.S.ref_value array

type tables = table ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let clone (tables : tables) : tables =
  let s = Env_id.Tbl.to_seq tables in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, Array.copy a)) s) )
       s

let convert_ref_values (v : Value.ref_value) : Sym_value.S.ref_value =
  match v with Funcref f -> Funcref f | _ -> assert false

let convert (orig_table : Table.t) : table =
  Array.map convert_ref_values orig_table.data

let get_env env_id tables =
  match Env_id.Tbl.find_opt tables env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add tables env_id t;
    t

let get_table env_id (orig_table : Table.t) (tables : tables) t_id =
  let env = get_env env_id tables in
  match ITbl.find_opt env t_id with
  | Some t -> t
  | None ->
    let t = convert orig_table in
    ITbl.add env t_id t;
    t
