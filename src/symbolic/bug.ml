(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type trap =
  { err : Result.err
  ; model : Smtml.Model.t
  ; state : Thread.t
  }

type assertion =
  { assertion : Symbolic_boolean.t
  ; model : Smtml.Model.t
  ; state : Thread.t
  }

type t =
  [ `Trap of trap
  | `Assertion of assertion
  | `Prune
  ]

let pp ppf = function
  | `Trap t -> Fmt.pf ppf "trap: %s" (Result.err_to_string t.err)
  | `Assertion a -> Fmt.pf ppf "assertion: %a" Symbolic_boolean.pp a.assertion
  | `Prune -> Fmt.pf ppf "prune"

let is_trap = function `Assertion _ | `Prune -> false | `Trap _ -> true

let is_assertion = function `Assertion _ -> true | `Prune | `Trap _ -> false

let is_prune = function `Prune -> true | `Assertion _ | `Trap _ -> false

let get_state : [ `Trap of trap | `Assertion of assertion ] -> Thread.t =
  function
  | `Trap { state; _ } | `Assertion { state; _ } -> state

let get_model : [ `Trap of trap | `Assertion of assertion ] -> Smtml.Model.t =
  function
  | `Trap { model; _ } | `Assertion { model; _ } -> model

let compare_breadcrumbs (bug1 : [ `Trap of trap | `Assertion of assertion ])
  (bug2 : [ `Trap of trap | `Assertion of assertion ]) =
  let s1 = get_state bug1 in
  let s2 = get_state bug2 in
  let breadcrumbs1 = List.rev @@ s1.breadcrumbs in
  let breadcrumbs2 = List.rev @@ s2.breadcrumbs in
  List.compare compare breadcrumbs1 breadcrumbs2

let sort_seq_if b (seq : [ `Trap of trap | `Assertion of assertion ] Seq.t) =
  if b then List.of_seq seq |> List.sort compare_breadcrumbs |> List.to_seq
  else seq
