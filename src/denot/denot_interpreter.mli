(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: why is this taking a Result.t ?! *)
val run : Binary.Module.t Result.t -> unit Result.t
