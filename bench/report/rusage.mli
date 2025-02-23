type t =
  { clock : float
  ; utime : float  (** user CPU time used *)
  ; stime : float  (** system CPU time used *)
  ; maxrss : int64  (** Maximum resident size (in kilobytes) *)
  }

val pp : t Fmt.t
