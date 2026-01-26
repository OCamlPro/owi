(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Prelude.Result
include Syntax

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

let[@inline] iarray_iter f a =
  let err = ref None in
  try
    for i = 0 to Iarray.length a - 1 do
      match f (Iarray.unsafe_get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok () -> ()
    done;
    Ok ()
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] iarray_iteri f a =
  let err = ref None in
  try
    for i = 0 to Iarray.length a - 1 do
      match f i (Iarray.unsafe_get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok () -> ()
    done;
    Ok ()
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] iarray_map f a =
  let err = ref None in
  try
    ok
    @@ Iarray.init (Iarray.length a) (fun i ->
      let v = Iarray.get a i in
      match f v with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok v -> v )
  with Exit -> ( match !err with None -> assert false | Some e -> e )

let[@inline] iarray_fold_left f acc l =
  Iarray.fold_left
    (fun acc v ->
      let* acc in
      f acc v )
    (Ok acc) l
