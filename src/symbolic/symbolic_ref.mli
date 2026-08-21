(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Ref_intf.T
    with type 'value array_obj = unit
     and type 'value struct_obj = unit
     and type i32 = Symbolic_i32.t
