type t =
  { clock : float
  ; utime : float
  ; stime : float
  ; maxrss : int64
  }

val pp : t Fmt.t
