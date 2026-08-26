(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type i32 = Symbolic_i32.t

type boolean = Symbolic_boolean.t

type 'value t = |

let get_type _a = assert false

let new_fill _type_id _v _n = assert false

let new_fixed_with _type_id _fields = assert false

let get_elem _a _index = assert false

let set_elem _a _index _v = assert false

let length _a = assert false

let phys_equal _a1 _a2 = assert false
