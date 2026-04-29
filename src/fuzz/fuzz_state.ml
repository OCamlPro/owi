(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let model : Concrete_value.t list ref = ref []

let reset () = model := []

let brk = ref @@ Int32.of_int 0
