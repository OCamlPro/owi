(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t

val return : 'a -> 'a t

val bind : ('a -> 'b t) -> 'a t -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val map_state : (Abstract_state.t -> Abstract_state.t option) -> unit t

val fold_state : (Abstract_state.t -> 'a) -> 'a t

val run : 'a t -> Abstract_state.t -> ('a * Abstract_state.t) option
