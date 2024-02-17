(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type t = Symbolic_value.ref_value array

type collection = t ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let clone collection =
  (* TODO: this is ugly and should be rewritten *)
  let s = Env_id.Tbl.to_seq collection in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, Array.copy a)) s) )
       s

let convert_ref_values (v : Concrete_value.ref_value) : Symbolic_value.ref_value
    =
  match v with Funcref f -> Funcref f | _ -> assert false

let convert (orig_table : Concrete_table.t) =
  Array.map convert_ref_values orig_table.data

let get_env env_id tables =
  match Env_id.Tbl.find_opt tables env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add tables env_id t;
    t

let get_table env_id (orig_table : Concrete_table.t) collection t_id =
  let env = get_env env_id collection in
  match ITbl.find_opt env t_id with
  | Some t -> t
  | None ->
    let t = convert orig_table in
    ITbl.add env t_id t;
    t

let get t i = t.(i)

let set t i v = t.(i) <- v

let size t = Array.length t

let typ _t =
  (* TODO: add type to table *)
  (Types.Null, Types.Func_ht)

let max_size _t = assert false

let grow _t _new_size _x = assert false

let fill _t _pos _len _x = assert false

let copy ~t_src:_ ~t_dst:_ ~src:_ ~dst:_ ~len:_ = assert false
