type t =
  { clock : float
  ; utime : float
  ; stime : float
  ; maxrss : int64
  }

let pp fmt { clock; utime; stime; maxrss } =
  Fmt.pf fmt "%4.2f %4.2f %4.2f %Ld" clock utime stime maxrss
