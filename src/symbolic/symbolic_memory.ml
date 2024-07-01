(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

[@@@warning "-69"]

let page_size = Symbolic_value.const_i32 65_536l

type record =
  { address : Symbolic_value.int32
  ; value : Symbolic_value.int32
  ; condition : Symbolic_value.int32
  ; time : int
  }

type t =
  { size : Symbolic_value.int32
  ; mutable records : record list
  ; mutable time : int
  }

let create size =
  { size = Symbolic_value.const_i32 size; records = []; time = 0 }

let grow _m _delta = assert false

let size { size; _ } = Symbolic_value.I32.mul size page_size

let size_in_pages { size; _ } = size

let fill _ = assert false

let blit _ = assert false

let blit_string _m _str ~src:_ ~dst:_ ~len:_ = assert false

let clone _ = assert false

let load_8_s _ = assert false

let load_8_u _ = assert false

let load_16_s _ = assert false

let load_16_u _ = assert false

let load_32 mem addr =
  let open Symbolic_value in
  let v = const_i32 0l in
  List.fold_right
    (fun r acc ->
      Bool.select_expr
        (Bool.and_ (I32.eq addr r.address) r.condition)
        ~if_true:r.value ~if_false:acc )
    mem.records v

let load_64 _ = assert false

let store_8 _ ~addr:_ _ = assert false

let store_16 _ ~addr:_ _ = assert false

let store_32 mem ~addr v =
  mem.time <- succ mem.time;
  let record =
    { address = addr
    ; value = v
    ; condition = Smtml.Expr.Bool.true_
    ; time = mem.time
    }
  in
  mem.records <- record :: mem.records

let store_64 _ ~addr:_ _ = assert false

let get_limit_max _m = None (* TODO *)

let is_out_of_bounds _ = assert false

let free _ = assert false

let replace_size _ = assert false

let pp_v = Smtml.Expr.pp

let pp_record fmt { address; value; condition; time } =
  Format.pp fmt "(%a, %a, %a, %d)" pp_v address pp_v value pp_v condition time

let pp fmt { time; records; _ } =
  Format.pp fmt "Time: %d@\n" time;
  Format.pp fmt "Records:@\n@[<hov 2>  %a@]"
    (Format.pp_list ~pp_sep:Format.pp_newline pp_record)
    records

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type collection = t ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let iter f collection = Env_id.Tbl.iter (fun _ tbl -> f tbl) collection

let clone collection =
  (* TODO: this is ugly and should be rewritten *)
  let s = Env_id.Tbl.to_seq collection in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, clone a)) s) )
       s

let convert (orig_mem : Concrete_memory.t) : t =
  let s = Concrete_memory.size_in_pages orig_mem in
  create s

let get_env env_id memories =
  match Env_id.Tbl.find_opt memories env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add memories env_id t;
    t

let get_memory env_id (orig_memory : Concrete_memory.t) collection g_id =
  let env = get_env env_id collection in
  match ITbl.find_opt env g_id with
  | Some t -> t
  | None ->
    let t = convert orig_memory in
    ITbl.add env g_id t;
    t
