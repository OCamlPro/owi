(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val ( let* ) :
     ('a, 'err) Prelude.Result.t
  -> ('a -> ('b, 'err) Prelude.Result.t)
  -> ('b, 'err) Prelude.Result.t

val ( let+ ) :
  ('a, 'err) Prelude.Result.t -> ('a -> 'b) -> ('b, 'err) Prelude.Result.t

val error : string -> ('a, string) Prelude.Result.t

val ok : 'a -> ('a, 'err) Prelude.Result.t

val list_iter :
     ('a -> (unit, 'err) Prelude.Result.t)
  -> 'a list
  -> (unit, 'err) Prelude.Result.t

val list_map :
     ('a -> ('b, 'err) Prelude.Result.t)
  -> 'a list
  -> ('b list, 'err) Prelude.Result.t

val list_fold_left :
     ('a -> 'b -> ('a, 'err) Prelude.Result.t)
  -> 'a
  -> 'b list
  -> ('a, 'err) Prelude.Result.t

val array_iter :
     ('a -> (unit, 'err) Prelude.Result.t)
  -> 'a array
  -> (unit, 'err) Prelude.Result.t

val array_map :
     ('a -> ('b, 'err) Prelude.Result.t)
  -> 'a array
  -> ('b array, 'err) Prelude.Result.t

val array_fold_left :
     ('a -> 'b -> ('a, 'err) Prelude.Result.t)
  -> 'a
  -> 'b array
  -> ('a, 'err) Prelude.Result.t
