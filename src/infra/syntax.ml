(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Prelude.Result

let[@inline] ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let[@inline] ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let[@inline] ok v = Ok v

let[@inline] list_iter f l =
  let rec aux = function
    | [] -> Ok ()
    | x :: xs -> ( match f x with Ok () -> aux xs | Error _ as e -> e )
  in
  aux l

let[@inline] list_map f l =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
      match f x with Ok v -> aux (v :: acc) xs | Error _ as e -> e )
  in
  aux [] l

let[@inline] list_concat_map f l =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | v :: tl -> (
      match f v with
      | Ok vs -> aux (List.rev_append vs acc) tl
      | Error _ as e -> e )
  in
  aux [] l

let[@inline] list_fold_left f acc l =
  let rec aux acc = function
    | [] -> Ok acc
    | v :: tl -> (
      match f acc v with Ok acc -> aux acc tl | Error _ as e -> e )
  in
  aux acc l

let[@inline] list_fold_left_map f acc l =
  let+ acc, l =
    list_fold_left
      (fun (acc, l) v ->
        let+ acc, x = f acc v in
        (acc, x :: l) )
      (acc, []) l
  in
  (acc, List.rev l)

let[@inline] array_iter f a =
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
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] array_iteri f a =
  let err = ref None in
  try
    for i = 0 to Array.length a - 1 do
      match f i (Array.unsafe_get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok () -> ()
    done;
    Ok ()
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] dynarray_iter f a =
  let err = ref None in
  try
    for i = 0 to Dynarray.length a - 1 do
      match f (Dynarray.get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok () -> ()
    done;
    Ok ()
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] dynarray_iteri f a =
  let err = ref None in
  try
    for i = 0 to Dynarray.length a - 1 do
      match f i (Dynarray.get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok () -> ()
    done;
    Ok ()
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] array_map f a =
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
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] dynarray_map f a =
  let err = ref None in
  try
    ok
    @@ Dynarray.init (Dynarray.length a) (fun i ->
      let v = Dynarray.get a i in
      match f v with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok v -> v )
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] array_fold_left f acc l =
  Array.fold_left
    (fun acc v ->
      let* acc in
      f acc v )
    (Ok acc) l

let[@inline] dynarray_fold_left f acc l =
  Dynarray.fold_left
    (fun acc v ->
      let* acc in
      f acc v )
    (Ok acc) l

let[@inline] dynarray_fold_lefti f acc l =
  snd
  @@ Dynarray.fold_left
       (fun (i, acc) v ->
         ( succ i
         , let* acc in
           f i acc v ) )
       (0, Ok acc) l
