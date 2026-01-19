(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module IMap = Map.Make (Int)

type t = Symbolic_table0.t =
  { data : Symbolic_ref.t IMap.t
  ; limits : Text.limits
  ; typ : Text.ref_type
  ; env_id : int
  ; id : int
  }

let get t i =
  match IMap.find_opt i t.data with Some v -> v | None -> assert false

let replace tbl = Symbolic_choice.modify_thread (Thread.replace_table tbl)

let set tbl i v =
  let data = IMap.add i v tbl.data in
  let tbl = { tbl with data } in
  replace tbl

let size t = IMap.cardinal t.data

let typ t = t.typ

let max_size t = t.limits.max

let grow _t _new_size _x =
  (* TODO
     let new_size = Int32.to_int new_size in
     let new_table = Array.make new_size x in
     Array.blit t.data 0 new_table 0 (Array.length t.data);
     t.data <- new_table
  *)
  Symbolic_choice.return ()

let fill t pos len x =
  let pos = Int32.to_int pos in
  let len = Int32.to_int len in
  let rec loop i data =
    if i < pos + len then
      let data = IMap.add i x t.data in
      loop (i + 1) data
    else { t with data }
  in
  loop pos t.data |> replace

let copy ~t_src ~t_dst ~src ~dst ~len =
  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in
  let rec loop i j l src_map dst_map =
    if l > 0 then
      let dst_map =
        match IMap.find_opt i src_map with
        | Some v -> IMap.add j v dst_map
        | None -> dst_map
      in
      loop (i + 1) (j + 1) (l - 1) src_map dst_map
    else { t_dst with data = dst_map }
  in
  loop src dst len t_src.data t_dst.data |> replace

let convert_ref_values (v : Concrete_ref.t) : Symbolic_ref.t =
  match v with Func f -> Func f | _ -> assert false

let of_concrete ~env_id ~id (original : Concrete_table.t) =
  let _i, data =
    Array.fold_left
      (fun (i, map) v ->
        let v = convert_ref_values v in
        let map = IMap.add i v map in
        (succ i, map) )
      (0, IMap.empty) original.data
  in
  { data; limits = original.limits; typ = original.typ; env_id; id }
