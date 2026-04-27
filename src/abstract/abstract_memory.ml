(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

type i32

type i64

type v128

type 'a choice

let load_8_s _m _i = assert false

let load_8_u _m _i = assert false

let load_16_s _m _i = assert false

let load_16_u _m _i = assert false

let load_32 _m _i = assert false

let load_64 _m _i = assert false

let load_128 _m _i = assert false

let store_8 _m ~addr:_ _i = assert false

let store_16 _m ~addr:_ _i = assert false

let store_32 _m ~addr:_ _i = assert false

let store_64 _m ~addr:_ _i = assert false

let store_128 _m ~addr:_ _i = assert false

let grow _m _i = assert false

let fill _m ~pos:_ ~len:_ _c = assert false

let blit ~src:_ ~src_idx:_ ~dst:_ ~dst_idx:_ ~len:_ = assert false

let blit_string _m _str ~src:_ ~dst:_ ~len:_ = assert false

let size _m = assert false

let size_in_pages _m = assert false

let get_limit_max _m = assert false
