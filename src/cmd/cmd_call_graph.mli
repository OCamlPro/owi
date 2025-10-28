(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type mode =
  | Complete
  | Sound

val build_call_graph_from_text_module :
     mode
  -> Text.modul
  -> string option
  -> (Types.binary Types.instr Annotated.t list Graph.t option * bool) Graph.t

val compute_distances : Binary.Module.t -> string option -> unit

val cmd :
     call_graph_mode:mode
  -> source_file:Fpath.t
  -> entry_point:string option
  -> scc:bool
  -> unit Result.t
