(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: implement this whole thing properly *)
type t =
  { data : Abstract_value.t Abstract_ref.t Iarray.t
  ; typ : Binary.ref_type
  }

let get_min : Binary.Table.Type.limits -> int = function
  | I32 { min; _ } -> Int32.to_int min
  | I64 { min; _ } -> Int64.to_int min

let init (typ : Binary.Table.Type.t) =
  let limits, ((_null, heap_type) as typ) = typ in
  let size = get_min limits in
  let data = Iarray.init size (fun _i -> Abstract_ref.null heap_type) in
  { data; typ }

let size _ = assert false

let get_type _ = assert false

let pp ppf _ = Fmt.pf ppf ""

let copy ~t_src:_ ~t_dst:_ ~src:_ ~dst:_ = assert false

let grow _ = assert false

let fill _ = assert false

let set t i v =
  let data = Iarray.mapi (fun i' v' -> if i' = i then v else v') t.data in
  { t with data }

let get t i = Iarray.get t.data i

let typ t = t.typ

let max_size _ = assert false
