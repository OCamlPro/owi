type prop = string

type t =
  { func : string
  ; preconditions : prop list
  ; postconditions : prop list
  }

val parse : Annot.t -> t Result.t
