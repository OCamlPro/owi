(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Wasm_ffi_intf.S with type extern_func = Concolic.P.Extern_func.extern_func
