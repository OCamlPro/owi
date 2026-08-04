(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = {
  total : int Atomic.t;
  completed : int Atomic.t;
  mutable enabled : bool;
}

let create () =
  { total = Atomic.make 0; completed = Atomic.make 0; enabled = false }

let enable t = t.enabled <- true

let increment_total t =
  Atomic.incr t.total

let increment_completed t =
  Atomic.incr t.completed

let get_total t =
  Atomic.get t.total

let get_completed t =
  Atomic.get t.completed

let bar_width = 80

let render_bar ~total ~completed =
  let width = bar_width - 20 in
  let percent =
    if total = 0 then 0.0
    else (float_of_int completed /. float_of_int total) *. 100.0
  in
  let filled = int_of_float ((percent /. 100.0) *. float_of_int width) in
  let bar =
    Fmt.str "%s%s" (String.make filled '#') (String.make (width - filled) ' ')
  in
  Logs.app (fun m ->
      m "\r[%s] %3.0f%% (%d/%d tasks)" bar percent completed total)

let report t =
  if t.enabled then
    let total = get_total t in
    let completed = get_completed t in
    if total = 0 then
      Logs.app (fun m -> m "Waiting for tasks...")
    else if Unix.isatty Unix.stdout then
      render_bar ~total ~completed
    else
      Logs.app (fun m -> m "Progress: %d / %d tasks completed" completed total)

let finish t =
  if t.enabled then
    let total = get_total t in
    let completed = get_completed t in
    if Unix.isatty Unix.stdout then (
      (* The final bar was already printed by the last call to report.
         Just move to a new line so subsequent logs appear cleanly. *)
      Logs.app (fun m -> m "")
    ) else
      Logs.app (fun m -> m "Done: %d / %d tasks completed" completed total)