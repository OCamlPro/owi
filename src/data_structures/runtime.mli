type ('a, 'b) t =
  | Local of 'a
  | Imported of 'b Imported.t
