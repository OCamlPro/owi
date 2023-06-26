type t = int

module IMap = Map.Make (Int)

type 'a collection =
  { c : 'a IMap.t
  ; last : int
  }

let empty = { c = IMap.empty; last = 0 }

let with_fresh_id f c =
  let open Syntax in
  let last = c.last in
  let* e, r = f last in
  Ok ({ c = IMap.add c.last e c.c; last = c.last + 1 }, r)

let get i c = IMap.find i c.c

let pp ppf i = Format.fprintf ppf "f_%i" i
