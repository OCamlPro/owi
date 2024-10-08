(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Prelude.Result

let ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let ok v = Ok v

let list_iter f l =
  let err = ref None in
  try
    List.iter
      (fun v ->
        match f v with
        | Error _e as e ->
          err := Some e;
          raise Exit
        | Ok () -> () )
      l;
    Ok ()
  with Exit -> Option.get !err

let list_map f l =
  let err = ref None in
  try
    ok
    @@ List.map
         (fun v ->
           match f v with
           | Error _e as e ->
             err := Some e;
             raise Exit
           | Ok v -> v )
         l
  with Exit -> Option.get !err

let list_concat_map f l =
  let err = ref None in
  try
    ok
    @@ List.concat_map
         (fun v ->
           match f v with
           | Error _e as e ->
             err := Some e;
             raise Exit
           | Ok v -> v )
         l
  with Exit -> Option.get !err

let list_fold_left f acc l =
  List.fold_left
    (fun acc v ->
      let* acc in
      f acc v )
    (Ok acc) l

let list_fold_left_map f acc l =
  let+ acc, l =
    list_fold_left
      (fun (acc, l) v ->
        let+ acc, x = f acc v in
        (acc, x :: l) )
      (acc, []) l
  in
  (acc, List.rev l)

let array_iter f a =
  let err = ref None in
  try
    for i = 0 to Array.length a - 1 do
      match f (Array.unsafe_get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok () -> ()
    done;
    Ok ()
  with Exit -> Option.get !err

let array_map f a =
  let err = ref None in
  try
    ok
    @@ Array.init (Array.length a) (fun i ->
           let v = Array.get a i in
           match f v with
           | Error _e as e ->
             err := Some e;
             raise Exit
           | Ok v -> v )
  with Exit -> Option.get !err

let array_fold_left f acc l =
  Array.fold_left
    (fun acc v ->
      let* acc in
      f acc v )
    (Ok acc) l
