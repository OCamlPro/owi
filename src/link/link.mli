(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to link a binary/extern module and producing a runnable module along
    with a link state. *)

module Concrete :
  Link_intf.T
    with type extern_func := Concrete_extern.Func.t
     and type extern_module := Concrete_extern.Module.t
     and type data := Concrete_data.t

module Symbolic :
  Link_intf.T
    with type extern_func := Symbolic_extern.Func.t
     and type extern_module := Symbolic_extern.Module.t
     and type data := Symbolic_data.t

module Abstract :
  Link_intf.T
    with type extern_func := Abstract_extern.Func.t
     and type extern_module := Abstract_extern.Module.t
     and type data := (* TODO *) string
