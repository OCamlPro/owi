(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Wasm_ffi_intf.S with type extern_func = Symbolic_extern_func.extern_func

val wasi_snapshot_preview1 : extern_func Extern.Module.t
