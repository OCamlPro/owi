(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.boolean

val false_ : Abstract_domain.Context.t -> t

val true_ : Abstract_domain.Context.t -> t

val of_bool : Abstract_domain.Context.t -> bool -> t

val not : Abstract_domain.Context.t -> t -> t

val or_ : Abstract_domain.Context.t -> t -> t -> t

val and_ : Abstract_domain.Context.t -> t -> t -> t

val pp : t Fmt.t

val equal : t -> t -> bool
