(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

let ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let error v = Error v

let error_s format = Format.kasprintf error format

let ok v = Ok v

let list_iter f l =
  let exception E of string in
  try
    List.iter
      (fun v -> match f v with Error msg -> raise (E msg) | Ok () -> ())
      l;
    Ok ()
  with E msg -> Error msg

let list_map f l =
  let exception E of string in
  try
    ok
    @@ List.map (fun v -> match f v with Error s -> raise (E s) | Ok v -> v) l
  with E s -> Error s

let list_fold_left (f : 'a -> 'b -> 'a Result.t) (acc : 'a) (l : 'b list) :
  'a Result.t =
  List.fold_left
    (fun acc v ->
      let* acc in
      f acc v )
    (Ok acc) l

let array_iter f a =
  let exception E of string in
  try
    for i = 0 to Array.length a - 1 do
      match f (Array.unsafe_get a i) with
      | Error msg -> raise (E msg)
      | Ok () -> ()
    done;
    Ok ()
  with E msg -> Error msg

let array_map f a =
  let exception E of string in
  try
    ok
    @@ Array.init (Array.length a) (fun i ->
         let v = Array.get a i in
         match f v with Error s -> raise (E s) | Ok v -> v )
  with E s -> Error s

let array_fold_left f acc a =
  let exception E of string in
  let acc = ref acc in
  try
    for i = 0 to Array.length a - 1 do
      match f !acc (Array.unsafe_get a i) with
      | Error msg -> raise (E msg)
      | Ok v -> acc := v
    done;
    Ok !acc
  with E e -> Error e
