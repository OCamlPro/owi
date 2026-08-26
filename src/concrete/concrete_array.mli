(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Array_intf.T
    with type i32 = Concrete_i32.t
     and type boolean = Concrete_boolean.t
