(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type mode =
  | Complete
  | Sound

val build_call_graph_from_text_module :
  mode -> Text.Module.t -> string option -> Call_graph.t

val compute_distances : Binary.Module.t -> string option -> unit

val cmd :
     call_graph_mode:mode
  -> source_file:Fpath.t
  -> entry_point:string option
  -> unit Result.t
