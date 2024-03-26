(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type t =
  { mutable data : Symbolic_value.ref_value array
  ; limits : limits
  ; typ : simplified ref_type
  }

let clone_t { limits; data; typ } = { typ; limits; data = Array.copy data }

type collection = t ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let clone (collection : collection) =
  (* TODO: this is ugly and should be rewritten *)
  let s = Env_id.Tbl.to_seq collection in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, clone_t a)) s) )
       s

let convert_ref_values (v : Concrete_value.ref_value) : Symbolic_value.ref_value
    =
  match v with Funcref f -> Funcref f | _ -> assert false

let convert (orig_table : Concrete_table.t) =
  { data = Array.map convert_ref_values orig_table.data
  ; limits = orig_table.limits
  ; typ = orig_table.typ
  }

let get_env env_id tables =
  match Env_id.Tbl.find_opt tables env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add tables env_id t;
    t

let get_table env_id (orig_table : Concrete_table.t) (collection : collection)
  t_id =
  let env = get_env env_id collection in
  match ITbl.find_opt env t_id with
  | Some t -> t
  | None ->
    let t = convert orig_table in
    ITbl.add env t_id t;
    t

let get t i = t.data.(i)

let set t i v = t.data.(i) <- v

let size t = Array.length t.data

let typ t = t.typ

let max_size t = t.limits.max

let grow t new_size x =
  let new_size = Int32.to_int new_size in
  let new_table = Array.make new_size x in
  Array.blit t.data 0 new_table 0 (Array.length t.data);
  t.data <- new_table

let fill t pos len x =
  let pos = Int32.to_int pos in
  let len = Int32.to_int len in
  Array.fill t.data pos len x

let copy ~t_src ~t_dst ~src ~dst ~len =
  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in
  Array.blit t_src.data src t_dst.data dst len
