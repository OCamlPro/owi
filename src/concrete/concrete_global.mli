(** runtime global *)

open Types

type t =
  { mutable value : Concrete_value.t
  ; label : string option
  ; mut : mut
  ; typ : simplified val_type
  }

val value : t -> Concrete_value.t

val set_value : t -> Concrete_value.t -> unit

val typ : t -> simplified val_type

val mut : t -> mut
