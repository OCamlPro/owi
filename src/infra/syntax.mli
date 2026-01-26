(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val ( let* ) :
     ('a, 'err) Prelude.Result.t
  -> ('a -> ('b, 'err) Prelude.Result.t)
  -> ('b, 'err) Prelude.Result.t

val ( let+ ) :
  ('a, 'err) Prelude.Result.t -> ('a -> 'b) -> ('b, 'err) Prelude.Result.t

val list_iter :
     ('a -> (unit, 'err) Prelude.Result.t)
  -> 'a list
  -> (unit, 'err) Prelude.Result.t

val list_map :
     ('a -> ('b, 'err) Prelude.Result.t)
  -> 'a list
  -> ('b list, 'err) Prelude.Result.t

val list_concat_map :
     ('a -> ('b list, 'err) Prelude.Result.t)
  -> 'a list
  -> ('b list, 'err) Prelude.Result.t

val list_fold_left :
     ('a -> 'b -> ('a, 'err) Prelude.Result.t)
  -> 'a
  -> 'b list
  -> ('a, 'err) Prelude.Result.t

val list_fold_left_map :
     ('a -> 'b -> ('a * 'c, 'err) Prelude.Result.t)
  -> 'a
  -> 'b list
  -> ('a * 'c list, 'err) Prelude.Result.t

val iarray_iter :
     ('a -> (unit, 'err) Prelude.Result.t)
  -> 'a Iarray.t
  -> (unit, 'err) Prelude.Result.t

val iarray_iteri :
     (int -> 'a -> (unit, 'err) Prelude.Result.t)
  -> 'a Iarray.t
  -> (unit, 'err) Prelude.Result.t

val iarray_map :
     ('a -> ('b, 'err) Prelude.Result.t)
  -> 'a Iarray.t
  -> ('b Iarray.t, 'err) Prelude.Result.t

val iarray_fold_left :
     ('a -> 'b -> ('a, 'err) Prelude.Result.t)
  -> 'a
  -> 'b Iarray.t
  -> ('a, 'err) Prelude.Result.t
