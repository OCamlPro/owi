(** the types of imported values *)
type 'a t =
  { modul : string
  ; name : string
  ; assigned_name : string option
  ; desc : 'a
  }
