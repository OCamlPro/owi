type t = int

module IMap = Map.Make (Int)

type 'a collection =
  { c : 'a IMap.t
  ; last : int
  }

let empty = { c = IMap.empty; last = 0 }

let add f c = (c.last, { c = IMap.add c.last f c.c; last = c.last + 1 })

let get i c = IMap.find i c.c

let pp ppf i =
  Format.fprintf ppf "f_%i" i
