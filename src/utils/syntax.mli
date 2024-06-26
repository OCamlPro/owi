(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val ( let* ) :
     ('a, 'err) Stdlib.Result.t
  -> ('a -> ('b, 'err) Stdlib.Result.t)
  -> ('b, 'err) Stdlib.Result.t

val ( let+ ) :
  ('a, 'err) Stdlib.Result.t -> ('a -> 'b) -> ('b, 'err) Stdlib.Result.t

val error : string -> ('a, string) Stdlib.Result.t

val error_s :
  ('a, Format.formatter, unit, ('b, string) Stdlib.Result.t) format4 -> 'a

val ok : 'a -> ('a, 'err) Stdlib.Result.t

val list_iter :
     ('a -> (unit, 'err) Stdlib.Result.t)
  -> 'a list
  -> (unit, 'err) Stdlib.Result.t

val list_map :
     ('a -> ('b, 'err) Stdlib.Result.t)
  -> 'a list
  -> ('b list, 'err) Stdlib.Result.t

val list_fold_left :
     ('a -> 'b -> ('a, 'err) Stdlib.Result.t)
  -> 'a
  -> 'b list
  -> ('a, 'err) Stdlib.Result.t

val array_iter :
     ('a -> (unit, 'err) Stdlib.Result.t)
  -> 'a array
  -> (unit, 'err) Stdlib.Result.t

val array_map :
     ('a -> ('b, 'err) Stdlib.Result.t)
  -> 'a array
  -> ('b array, 'err) Stdlib.Result.t

val array_fold_left :
     ('a -> 'b -> ('a, 'err) Stdlib.Result.t)
  -> 'a
  -> 'b array
  -> ('a, 'err) Stdlib.Result.t
