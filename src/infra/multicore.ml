(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: remove once there is an equivalent in stdlib *)
let atomic_modify f atomic =
  let rec loop f atomic =
    let v_old = Atomic.get atomic in
    let v_new = f v_old in
    if Atomic.compare_and_set atomic v_old v_new then () else loop f atomic
  in
  loop f atomic
