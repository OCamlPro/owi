(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.boolean

val false_ : Abstract_domain.Context.t -> t

val true_ : Abstract_domain.Context.t -> t

val eq : Abstract_domain.Context.t -> t -> t -> t

val of_bool : Abstract_domain.Context.t -> bool -> t

val not : Abstract_domain.Context.t -> t -> t

val or_ : Abstract_domain.Context.t -> t -> t -> t

val and_ : Abstract_domain.Context.t -> t -> t -> t

val pp : Abstract_domain.Context.t -> t Fmt.t

val unknown : Abstract_domain.Context.t -> t

val can_be_true : Abstract_domain.Context.t -> t -> bool
