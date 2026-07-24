(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to link a binary/extern module and producing a runnable module along
    with a link state. *)

module Make (M : Link_intf.M) :
  Link_intf.T
    with type extern_func := M.extern_func
     and type extern_module = M.extern_module
     and type data := M.data
