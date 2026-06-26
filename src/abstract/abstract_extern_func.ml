(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Extern.Func.Make (Abstract_value) (Abstract_monad) (Abstract_memory)

type abs_extern_func = 
  | Assume
  | I32_symbol
  | I64_symbol

exception ExternFuncException of abs_extern_func
